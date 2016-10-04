package latis.ops.agg

import latis.dm._
import latis.metadata.Metadata

/**
 * Combine two georeferenced image datasets. The two datasets must share 
 * a common latitude or longitude set. Their data must also be in 
 * row major order (grouped by common latitude). 
 */
class ImageTileAggregation extends TileAggregation() {
  
  def getLatLon(s: Sample): (Double, Double) = (s.findVariableByName("row"), s.findVariableByName("col")) match {
    case (Some(Number(lat)), Some(Number(lon))) => (lat,lon)
    case _ => throw new Exception("Sample did not contain variables named 'latitude and 'longitude'.")
  }
  
  /**
   * Determine whether this is a horizontal or vertical aggregation and 
   * delegate to respective methods.
   */
  override def aggregate(dataset1: Dataset, dataset2: Dataset): Dataset = {
    val ds1 = dataset1.force
    val ds2 = dataset2.force
    
    val (func1, func2, samples1, samples2) = (ds1, ds2) match {
      case (Dataset(f1 @ Function(it1)), Dataset(f2 @ Function(it2))) => (f1, f2, it1.toSeq, it2.toSeq)
    }
    
    val metadata1 = func1.getMetadata
    val metadata2 = func2.getMetadata
    
    //val (lats1, lons1) = samples1.map(getLatLon).unzip
    //val (lats2, lons2) = samples2.map(getLatLon).unzip
    
    if(metadata1("miny") == metadata2("miny") && metadata1("maxy") == metadata2("maxy")) aggregateH(ds1, ds2, metadata1("nrow").toInt, metadata2("nrow").toInt)
    else if(metadata1("minx") == metadata2("minx") && metadata1("maxx") == metadata2("maxx")) aggregateV(ds1, ds2)
    else throw new IllegalArgumentException("The datasets are not aligned properly for tile aggregation.")
  }
  
  /**
   * Reorder the Datasets into row major order. And figure out how many
   * rows there should be.
   */
  private def orderTiles(dss: Seq[Dataset]) = {
    
    val minxys = dss.map { x => x match { case Dataset(f: Function) => (f.getMetadata("minx").get,f.getMetadata("miny").get) } }
    
    val ulcwithds = minxys zip dss
    
    val sortedulcwithds = ulcwithds.sortWith((a,b) => a._1._1.toDouble < b._1._1.toDouble)
    
    val orderedDSs = sortedulcwithds.map(f => f._2)
    
    val rownums = dss.map { x => x match { case Dataset(f: Function) => f.getMetadata("nrow").get.toInt }}
    val rowcount1 = Set(rownums: _*).toSeq.foldLeft(0)(_+_)
    /*
    val sits = dss.collect {case Dataset(Function(it)) => it.toSeq}
    
    val (lats, lons) = sits.map(_.map(getLatLon).unzip).unzip
    
    val zipped = dss.zip(lons).zip(lats)
    //group by row
    val x = zipped.groupBy(p => p._2.distinct)
    //sort the rows
    val y = x.toSeq.sortWith((a,b) => a._1.min > b._1.min)
    //simplify the type
    val rows = y.map(_._2.map(_._1))
    val rcount = rows.length
    
    //order each row by longitude
    val k = rows.map(row => row.sortWith((a,b) => a._2.min < b._2.min))
    //simplify to get ordered datasets
    (k.flatMap(_.map(_._1)), rcount)
    * 
    */
    (orderedDSs,rowcount1)
  }
  
  override def apply(datasets: Seq[Dataset]): Dataset = {
    val dss = datasets.map(_.force)
    val (ordered, rowcount) = orderTiles(dss)
    
    if (rowcount == 0) Dataset.empty
    else {
      val rows = ordered.grouped(dss.size/rowcount)
      //aggregate within each row
      val s = rows.map(_.reduceLeft(aggregate(_,_)))
      //aggregate the rows
      s.reduceLeft(aggregate(_,_))
    }
  }
  
  def aggregateH(ds1: Dataset, ds2: Dataset, dim1: Int, dim2: Int): Dataset = {
    val (it1, it2, f) = (ds1, ds2) match {
      case (Dataset(f @ Function(it1)), Dataset(Function(it2))) => (it1.grouped(dim1), it2.grouped(dim2), f)
    }
    
    val md = f.getMetadata("epsg") match {
      case Some(code) => Metadata(Map("epsg" -> code))
      case None => Metadata.empty
    }
    val it = it1.zip(it2).flatMap(p => p._1 ++ p._2).buffered
    
    Dataset(Function(it.head.domain, it.head.range, it, md))
  }
  
  def aggregateV(ds1: Dataset, ds2: Dataset): Dataset = {
    val (it1, it2, f) = (ds1, ds2) match {
      case (Dataset(f @ Function(it1)), Dataset(Function(it2))) => (it1, it2, f)
    }
    
    val md = f.getMetadata("epsg") match {
      case Some(code) => Metadata(Map("epsg" -> code))
      case None => Metadata.empty
    }
    val it = (it1 ++ it2).buffered
    
    Dataset(Function(it.head.domain, it.head.range, it, md))
  }
  
}

object ImageTileAggregation {

  def apply(): ImageTileAggregation = new ImageTileAggregation()
}
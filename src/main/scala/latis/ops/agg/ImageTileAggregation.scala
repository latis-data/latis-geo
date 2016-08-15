package latis.ops.agg

import latis.dm._
import latis.metadata.Metadata

/**
 * Combine two georeferenced image datasets. The two datasets must share 
 * a common latitude or longitude set. Their data must also be in 
 * row major order (grouped by common latitude). 
 */
class ImageTileAggregation extends TileAggregation() {
  
  def getLatLon(s: Sample): (Double, Double) = (s.findVariableByName("latitude"), s.findVariableByName("longitude")) match {
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
    
    val (samples1, samples2) = (ds1, ds2) match {
      case (Dataset(Function(it1)), Dataset(Function(it2))) => (it1.toSeq, it2.toSeq)
    }
    
    val (lats1, lons1) = samples1.map(getLatLon).unzip
    val (lats2, lons2) = samples2.map(getLatLon).unzip
    
    if(lats1.diff(lats2).isEmpty) aggregateH(ds1, ds2, lons1.distinct.size, lons2.distinct.size)
    else if(lons1.diff(lons2).isEmpty) aggregateV(ds1, ds2)
    else throw new IllegalArgumentException("The datasets are not aligned properly for tile aggregation.")
  }
  
  /**
   * Reorder the Datasets into row major order. And figure out how many
   * rows there should be.
   */
  private def orderTiles(dss: Seq[Dataset]) = {
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
  }
  
  override def apply(datasets: Seq[Dataset]): Dataset = {
    val dss = datasets.map(_.force)
    val (ordered, rowcount) = orderTiles(dss)
    
    val rows = ordered.grouped(dss.size/rowcount)
    //aggregate within each row
    val s = rows.map(_.reduceLeft(aggregate(_,_)))
    //aggregate the rows
    s.reduceLeft(aggregate(_,_))
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
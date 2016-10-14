package latis.ops.agg

import latis.dm._
import latis.metadata.Metadata

/**
 * Combine two georeferenced image datasets. The two datasets must share 
 * a common latitude or longitude set. Their data must also be in 
 * row major order (grouped by common latitude). 
 */
class ImageTileAggregation extends TileAggregation() {
  
//  def getLatLon(s: Sample): (Double, Double) = (s.findVariableByName("latitude"), s.findVariableByName("longitude")) match {
//    case (Some(Number(lat)), Some(Number(lon))) => (lat,lon)
//    case _ => throw new Exception("Sample did not contain variables named 'latitude and 'longitude'.")
//  }
  
  /**
   * Determine whether this is a horizontal or vertical aggregation and 
   * delegate to respective methods.
   */
  override def aggregate(dataset1: Dataset, dataset2: Dataset): Dataset = {
    //TODO: do we ever use this interface? or do we always throw a Seq of tiles to apply?
    val ds1 = dataset1.force
    val ds2 = dataset2.force
    
    val (func1, func2, samples1, samples2) = (ds1, ds2) match {
      case (Dataset(f1 @ Function(it1)), Dataset(f2 @ Function(it2))) => (f1, f2, it1.toSeq, it2.toSeq)
    }
    
    val metadata1 = func1.getMetadata
    val metadata2 = func2.getMetadata
    
    //val (lats1, lons1) = samples1.map(getLatLon).unzip
    //val (lats2, lons2) = samples2.map(getLatLon).unzip
    
    if(metadata1("minLat") == metadata2("minLat") && metadata1("maxLat") == metadata2("maxLat")) aggregateH(ds1, ds2)
    else if(metadata1("minLon") == metadata2("minLon") && metadata1("maxLon") == metadata2("maxLon")) aggregateV(ds1, ds2)
    else throw new IllegalArgumentException("The datasets are not aligned properly for tile aggregation.")
  }
  
  /**
   * Reorder the Datasets into row major order. And figure out how many
   * rows there should be.
   */
  private def minLat(ds: Dataset): Double = {
    ds match {
      case Dataset(f: Function) => f.getMetadata()("minLat").toDouble
    }
  }
  private def minLon(ds: Dataset): Double = {
    ds match {
      case Dataset(f: Function) => f.getMetadata()("minLon").toDouble
    }
  }
  private def orderTiles(dss: Seq[Dataset]) = {
    val t0 = System.nanoTime
    
    
    val x = dss.groupBy { x => minLat(x) } //group rows
    //x.foreach(f => println(f._1))
    val xx = x.map(f => f._2.sortBy { x => minLon(x) }) //sort by column within rows
    //xx.foreach { x => x.foreach { x => println(minLon(x)) } }
    
    //val minxys = dss.map { x => x match { case Dataset(f: Function) => (f.getMetadata("minLon").get,f.getMetadata("minLat").get) } }
    //val maxxys = dss.map { x => x match { case Dataset(f: Function) => (f.getMetadata("maxLon").get,f.getMetadata("maxLat").get) } }

    //minxys.foreach(f => println("min: (" + f._1 + "," + f._2 + ")"))
    //maxxys.foreach(f => println("max: (" + f._1 + "," + f._2 + ")"))

    //println("minxys length: " + minxys.length)
    //val ulcwithds = minxys zip dss 
    //println("ulcwithds length: " + ulcwithds.length)
    //val sortedulcwithds = ulcwithds.sortBy(t => t._1)
    
    //val orderedDSs = sortedulcwithds.map(f => f._2)
    
    //val rownums = dss.map { x => x match { case Dataset(f: Function) => f.getMetadata("nrow").get.toInt }}
    //val rowcount1 = 1
    
    
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
    val t1 = System.nanoTime
    println("orderTiles elapsed time: " + (t1 - t0) + "ns")
    (k.flatMap(_.map(_._1)), rcount)
    * 
    */
    
    val t1 = System.nanoTime
    println("orderTiles elapsed time: " + (t1 - t0) + "ns")
    //(orderedDSs,rowcount1)
    xx
    
    
  }
  
  override def apply(datasets: Seq[Dataset]): Dataset = {
    //val dss = datasets.map(_.force)
    //val (ordered, rowcount) = orderTiles(dss)
    println("apply called")
    val ordered = orderTiles(datasets) //effectively a 2D array: row varying slowest (outer dimension)

    //if (rowcount == 0) Dataset.empty
    //else {
      //val rows = ordered.grouped(dss.size/rowcount)
      //aggregate within each row
      val s = ordered.map(_.reduceLeft(aggregateH(_,_))).toSeq.reverse // need to reverse because aggregateH returns strips orderef from south to north
      //If we leave the data in the original (row,col) space, we shouldn't need to reverse here.
      //We can worry about that if/when we transform to a (lon,lat) grid
      
      //aggregate the rows
      s.reduceLeft(aggregateV(_,_))
    //}
  }
  
  def aggregateH(ds1: Dataset, ds2: Dataset): Dataset = {
    //assumes ds1 is positioned to the left of ds2
    val t0 = System.nanoTime
    
    val (it1, it2, f) = (ds1, ds2) match {
      case (Dataset(f @ Function(it1)), Dataset(Function(it2))) => {
        val ncol = f.getMetadata()("ncol").toInt
          (it1.grouped(ncol), it2.grouped(ncol), f) //TODO: shouldn't assume both have same ncol, e.g. tiling 3rd to the 1st 2
        }
    }
    
    val md = f.getMetadata("epsg") match {
      case Some(code) => Metadata(Map("epsg" -> code))
      case None => Metadata.empty
    }
    val it = it1.zip(it2).flatMap(p => p._1 ++ p._2).buffered //append pairs of rows
    
    val t1 = System.nanoTime
    println("aggregateH elapsed time: " + (t1 - t0) + "ns") //how much work is being done here? presumably the is not iterating yet
    //TODO: update nrow, ncol metadata? and length?
    Dataset(Function(it.head.domain, it.head.range, it, md))
  }
  /*
  def aggregateH(ds1: Dataset, ds2: Dataset, dim1: Int, dim2: Int): Dataset = {
    val t0 = System.nanoTime
    
    val (it1, it2, f) = (ds1, ds2) match {
      case (Dataset(f @ Function(it1)), Dataset(Function(it2))) => (it1.grouped(dim1), it2.grouped(dim2), f)
    }
    
    val md = f.getMetadata("epsg") match {
      case Some(code) => Metadata(Map("epsg" -> code))
      case None => Metadata.empty
    }
    val it = it1.zip(it2).flatMap(p => p._1 ++ p._2).buffered
    
    val t1 = System.nanoTime
    println("aggregateH elapsed time: " + (t1 - t0) + "ns")
    Dataset(Function(it.head.domain, it.head.range, it, md))
  }
  * 
  */
  
  def aggregateV(ds1: Dataset, ds2: Dataset): Dataset = {
    //assumes ds1 should be positioned above ds2 
    val (it1, it2, f) = (ds1, ds2) match {
      case (Dataset(f @ Function(it1)), Dataset(Function(it2))) => (it1, it2, f)
    }
    
    val md = f.getMetadata("epsg") match {
      case Some(code) => Metadata(Map("epsg" -> code))
      case None => Metadata.empty
    }
    val it = (it1 ++ it2).buffered
    
    //TODO: update nrow, ncol metadata? and length?
    Dataset(Function(it.head.domain, it.head.range, it, md))
  }
  
}

object ImageTileAggregation {

  def apply(): ImageTileAggregation = new ImageTileAggregation()
}
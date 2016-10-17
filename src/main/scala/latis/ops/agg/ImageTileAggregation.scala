package latis.ops.agg

import latis.dm._
import latis.metadata.Metadata
import scala.collection.immutable.TreeMap

/**
 * Combine two georeferenced image datasets. The two datasets must share 
 * a common latitude or longitude set. Their data must also be in 
 * row major order (grouped by common latitude). 
 */
class ImageTileAggregation extends TileAggregation() {
  /*
  def getLatLon(s: Sample): (Double, Double) = (s.findVariableByName("latitude"), s.findVariableByName("longitude")) match {
    case (Some(Number(lat)), Some(Number(lon))) => (lat,lon)
    case _ => throw new Exception("Sample did not contain variables named 'latitude and 'longitude'.")
  }
 *  
 */
  
  /**
   * Determine whether this is a horizontal or vertical aggregation and 
   * delegate to respective methods.
   *
  override def aggregate(dataset1: Dataset, dataset2: Dataset): Dataset = {
    //TODO: do we ever use this interface? or do we always throw a Seq of tiles to apply?
    val ds1 = dataset1.force
    val ds2 = dataset2.force
    
    val (func1, func2, samples1, samples2) = (ds1, ds2) match {
      case (Dataset(f1 @ Function(it1)), Dataset(f2 @ Function(it2))) => (f1, f2, it1.toSeq, it2.toSeq)
    }
    
    val metadata1 = func1.getMetadata
    val metadata2 = func2.getMetadata
    
    if(metadata1("minLat") == metadata2("minLat") && metadata1("maxLat") == metadata2("maxLat")) aggregateH(ds1, ds2)
    else if(metadata1("minLon") == metadata2("minLon") && metadata1("maxLon") == metadata2("maxLon")) aggregateV(ds1, ds2)
    else throw new IllegalArgumentException("The datasets are not aligned properly for tile aggregation.")
  }
  * 
  */
  
  /**
   * Reorder the Datasets into row major order. And figure out how many
   * rows there should be.
   */
  private def minLat(ds: Dataset): Double = {
    ds match {
      case Dataset(f: Function) => -1 * f.getMetadata()("minLat").toDouble // negate latitudes in order to sort them from north to south further below
    }
  }
  private def minLon(ds: Dataset): Double = {
    ds match {
      case Dataset(f: Function) => f.getMetadata()("minLon").toDouble
    }
  }
  private def orderTiles(dss: Seq[Dataset]) = {
    val groupsOfLat = dss.groupBy { x => minLat(x) } //group rows, not ordered
    val groupsOfSortedLat = TreeMap(groupsOfLat.toArray:_*) // create sorted tree of latitudes from north to south
    val tiles = groupsOfSortedLat.map(f => f._2.sortBy { x => minLon(x) }) //sort by column within latitude row
    tiles
  }
  
  override def apply(datasets: Seq[Dataset]): Dataset = {
    val ordered = orderTiles(datasets) //effectively a 2D array: row varying slowest (outer dimension)
    val s = ordered.map(_.reduceLeft(aggregateH(_,_)))
    //If we leave the data in the original (row,col) space, we shouldn't need to reverse here.
    //We can worry about that if/when we transform to a (lon,lat) grid
    //TODO: order rows here???    Tiles are already ordered by now
    //aggregate the rows
    s.reduceLeft(aggregateV(_,_))
  }
  
  def aggregateH(ds1: Dataset, ds2: Dataset): Dataset = {
    //assumes ds1 is positioned to the left of ds2
    val (it1, it2, f1, f2) = (ds1, ds2) match {
      case (Dataset(f1 @ Function(it1)), Dataset(f2 @ Function(it2))) => {
        val ncol = f1.getMetadata()("ncol").toInt
        val ncol2 = f2.getMetadata()("ncol").toInt
          (it1.grouped(ncol), it2.grouped(ncol2), f1, f2) //TODO: shouldn't assume both have same ncol, e.g. tiling 3rd to the 1st 2
        }
    }
    
    val md = f1.getMetadata("epsg") match {
      case Some(code) => Metadata(Map("epsg" -> code))
      case None => Metadata.empty
    }
    val it = it1.zip(it2).flatMap(p => p._1 ++ p._2).buffered //append pairs of rows    
    val ncol1 = f1.getMetadata()("ncol").toInt
    val nrow1 = f1.getMetadata()("nrow").toInt
    val ncol2 = f2.getMetadata()("ncol").toInt
    val nrow2 = f2.getMetadata()("nrow").toInt
    //how much work is being done here? presumably the is not iterating yet
    // update nrow, ncol metadata
    val sizeMD = Metadata("nrow" -> nrow1.toString, "ncol" -> (ncol1+ncol2).toString)
    //TODO: update length metadata
    Dataset(Function(it.head.domain, it.head.range, it, sizeMD ++ md))
  }
  
  def aggregateV(ds1: Dataset, ds2: Dataset): Dataset = {
    //assumes ds1 should be positioned above ds2 
    val (it1, it2, f1, f2) = (ds1, ds2) match {
      case (Dataset(f1 @ Function(it1)), Dataset(f2 @ Function(it2))) => (it1, it2, f1,f2)
    }
    
    val md = f1.getMetadata("epsg") match {
      case Some(code) => Metadata(Map("epsg" -> code))
      case None => Metadata.empty
    }
    val it = (it1 ++ it2).buffered
    val ncol1 = f1.getMetadata()("ncol").toInt
    val nrow1 = f1.getMetadata()("nrow").toInt
    val ncol2 = f2.getMetadata()("ncol").toInt
    val nrow2 = f2.getMetadata()("nrow").toInt
    // update nrow, ncol and length metadata
    val sizeMD = Metadata("nrow" -> (nrow1+nrow2).toString, "ncol" -> ncol1.toString())
    //TODO: update length metadata
    Dataset(Function(it.head.domain, it.head.range, it, sizeMD ++ md))
  }
  
}

object ImageTileAggregation {

  def apply(): ImageTileAggregation = new ImageTileAggregation()
}
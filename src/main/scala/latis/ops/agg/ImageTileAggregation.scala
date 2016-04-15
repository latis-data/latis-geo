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
  override def aggregate(dataset1: Dataset, dataset2: Dataset) = {
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
  
  def aggregateH(ds1: Dataset, ds2: Dataset, dim1: Int, dim2: Int) = {
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
  
  def aggregateV(ds1: Dataset, ds2: Dataset) = {
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
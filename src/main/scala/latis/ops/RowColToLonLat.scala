package latis.ops

import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import org.geotools.referencing.crs.DefaultGeocentricCRS
import org.opengis.referencing.crs.CoordinateReferenceSystem
import com.vividsolutions.jts.geom.Coordinate
import latis.dm._
import latis.dm.Number
import latis.dm.Real
import latis.dm.Sample
import latis.dm.Tuple
import latis.dm.Variable
import latis.metadata.Metadata
import latis.util.iterator.MappingIterator
import org.geotools.factory.Hints


class RowColToLonLat(minLon: Double, maxLon: Double, minLat: Double, maxLat: Double) extends Operation {
  //TODO: impl as a coord system transformation?
  
  //size of image, populated from Function metadata
  private var nrow = 0 //image height in pixels
  private var ncol = 0 //image width in pixels
  private var dlon = 0.0 //pixel width in degrees
  private var dlat = 0.0 //pixel height in degrees
  
  //Convert (row,col) Tuple to (lon,lat)
  //TODO: check scalar names
  override def applyToTuple(tuple: Tuple) = tuple match {
    case Tuple(Seq(Integer(row), Integer(col))) => {
      if (nrow == 0 || ncol == 0) throw new Error("nrow and ncol not defined")
      val lon = Real(Metadata("longitude"), minLon + dlon * col)
      val lat = Real(Metadata("latitude"), minLat + dlat * (nrow - row - 1)) //minLat is at bottom
      Some(Tuple(lon, lat))
    }
    case other => Some(other)
  }

  /**
   * Override to apply to domain and range
   */
  override def applyToSample(sample: Sample): Option[Sample] = sample match {
    case Sample(domain, range) => {
      for (d <- applyToVariable(domain); r <- applyToVariable(range)) yield Sample(d,r)
    }
  }
  
  override def applyToFunction(function: Function): Option[Variable] = {
    //Get image size for later use
    //TODO: error handling
    nrow = function.getMetadata("nrow").get.toInt
    ncol = function.getMetadata("ncol").get.toInt
    dlon = (maxLon - minLon) / ncol
    dlat = (maxLat - minLat) / nrow
    
    //Delegate to super then add coordinate system metadata
    super.applyToFunction(function) match {
      case Some(f: Function) => Some(Function(f.getDomain, f.getRange, f.iterator, f.getMetadata + ("epsg" -> "4326")))
      case other => other
    }
  }
  
}

object RowColToLonLat extends OperationFactory {
  
  /**
   * RowColToLonLat(minLon: Double, maxLon: Double, minLat: Double, maxLat: Double)
   */
  override def apply(args: Seq[String]) = new RowColToLonLat(args(0).toDouble, 
                                                               args(1).toDouble, 
                                                               args(2).toDouble, 
                                                               args(3).toDouble)
  
  def apply(minLon: Double, maxLon: Double, minLat: Double, maxLat: Double): RowColToLonLat = {
    new RowColToLonLat(minLon, maxLon, minLat, maxLat)
  }
}
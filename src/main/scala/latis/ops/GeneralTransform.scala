package latis.ops

import org.geotools.factory.Hints
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import org.geotools.referencing.crs.DefaultGeocentricCRS
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.opengis.referencing.crs.GeocentricCRS
import org.opengis.referencing.crs.GeographicCRS

import com.vividsolutions.jts.geom.Coordinate

import latis.dm.Function
import latis.dm.Number
import latis.dm.Real
import latis.dm.Tuple
import latis.dm.Variable
import latis.metadata.Metadata

/**
 * Transform from ECEF to WGS84 3D.
 */
class GeneralTransform(target: String = "EPSG:4979") extends Operation {
  
  Hints.putSystemDefault(Hints.FORCE_LONGITUDE_FIRST_AXIS_ORDER, true)
  
  val sourceCRS: CoordinateReferenceSystem = null
  val targetCRS: CoordinateReferenceSystem = CRS.decode(target) //WGS84 3D
  
  lazy val transform = CRS.findMathTransform(sourceCRS, targetCRS, true)
  
  //Note, applies to any tuple with 2/3 Numbers.
  //TODO: match name or metadata convention for geolocation
  override def applyToTuple(tuple: Tuple): Option[Variable] = {
    if(sourceCRS == null) super.applyToTuple(tuple)
    else {
      val src = (sourceCRS.getCoordinateSystem.getDimension, tuple) match {
        case (2, Tuple(Seq(Number(x), Number(y)))) => new Coordinate(x,y)
        case (3, Tuple(Seq(Number(x), Number(y), Number(z)))) => new Coordinate(x,y,z)
        case _ => return super.applyToTuple(tuple) //presumably some other Tuple in the dataset
      }
      val dst = JTS.transform(src, new Coordinate(), transform)
      
      val vals = dst.z match {
        case Double.NaN => Seq(dst.x, dst.y)
        case _ =>  Seq(dst.x, dst.y, dst.z)
      }
      val names = targetCRS match {
        case _: GeocentricCRS => Seq("x", "y", "z")
        case _: GeographicCRS => Seq("longitude", "latitude", "altitude")
      }
      val vars = vals.zip(names).map(p => Real(Metadata(p._2), p._1))
      Some(Tuple(vars))
    }
  }
  
  /**
   * Override to set the new CRS Metadata.
   */
  override def applyToFunction(function: Function): Option[Variable] = {
    val crs = function.getMetadata("crs") match {
      case None => null
      case Some("EPSG:4978") => DefaultGeocentricCRS.CARTESIAN
      case Some(code) => CRS.decode(code)
    }
    
    if(crs == null) {
      super.applyToFunction(function)
    } else if(CRS.equalsIgnoreMetadata(crs, sourceCRS)) super.applyToFunction(function) match {
      case Some(f: Function) => Some(Function(f.getDomain, f.getRange, f.iterator, f.getMetadata + ("crs" -> target)))
      case other => other
    } else new GeneralTransform(target){ override val sourceCRS = crs }.applyToFunction(function)
  }
  
}

object GeneralTransform extends OperationFactory {
  
  override def apply() = new GeneralTransform
  
  def apply(target: String) = new GeneralTransform(target)
  
  override def apply(args: Seq[String]) = new GeneralTransform(args.head)
    
}
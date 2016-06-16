package latis.ops

import org.geotools.geometry.jts.JTS
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
import latis.util.Crs
import org.geotools.referencing.crs.DefaultGeocentricCRS.CARTESIAN

/**
 * Transform from ECEF to WGS84 3D.
 */
class GeneralTransform(target: String = "EPSG:4979") extends Operation {
  
  val sourceCRS: CoordinateReferenceSystem = null
  val targetCRS: CoordinateReferenceSystem = target match {
    case "EPSG:4978" => CARTESIAN
    case e => Crs.decode(target) 
  }
  
  lazy val transform = Crs.findMathTransform(sourceCRS, targetCRS)
  
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
    val crs = Crs.getCrs(function)
    
    if(crs == null) {
      super.applyToFunction(function)
    } else if(Crs.equalsIgnoreMetadata(crs, sourceCRS)) super.applyToFunction(function) match {
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
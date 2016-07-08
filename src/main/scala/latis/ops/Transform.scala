package latis.ops

import org.geotools.factory.Hints
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.crs.DefaultGeocentricCRS
import org.opengis.referencing.crs.CoordinateReferenceSystem

import com.vividsolutions.jts.geom.Coordinate

import latis.dm.Function
import latis.dm.Number
import latis.dm.Real
import latis.dm.Tuple
import latis.dm.Variable
import latis.metadata.Metadata
import latis.util.Crs
import com.typesafe.scalalogging.LazyLogging

/**
 * Transform from ECEF to WGS84 3D.
 */
class Transform extends Operation with LazyLogging {

  val sourceCRS: CoordinateReferenceSystem = DefaultGeocentricCRS.CARTESIAN //EPSG:4978
  val targetCRS: CoordinateReferenceSystem = Crs.decode("EPSG:4979") //WGS84 3D

  lazy val transform = Crs.findMathTransform(sourceCRS, targetCRS)

  //Note, applies to any tuple with 3 Numbers.
  //TODO: match name or metadata convention for geolocation
  override def applyToTuple(tuple: Tuple) = tuple match {
    case Tuple(Seq(Number(x), Number(y), Number(z))) => {
      //println(CRS.getAxisOrder(sourceCRS)) //INAPPLICABLE
      //println(CRS.getAxisOrder(targetCRS)) //NORTH_EAST
      try {
        val coord = new Coordinate(x, y, z)
        val tcoord = JTS.transform(coord, null, transform)
        val tup = Tuple(Real(Metadata("longitude"), tcoord.x),
          Real(Metadata("latitude"), tcoord.y),
          Real(Metadata("altitude"), tcoord.z))
        Some(tup)
      } catch {
        case e: Exception => {
          logger.warn(s"Failed to transform the coordinates: $x, $y, $z", e)
          //TODO: return None?
          //assume these shouldn't have been transformed
          super.applyToTuple(tuple)
        }
      }
    }
    case _ => super.applyToTuple(tuple) //presumably some other Tuple in the dataset
    //throw new UnsupportedOperationException("The Transform Operation expects a Tuple(x,y,z).")
  }

  /**
   * Override to set the new CRS Metadata.
   */
  override def applyToFunction(function: Function): Option[Variable] = {
    super.applyToFunction(function) match {
      case Some(f: Function) => Some(Function(f.getDomain, f.getRange, f.iterator, f.getMetadata + ("crs" -> "EPSG:4979")))
      case other => other
    }
  }

}

object Transform extends OperationFactory {

  override def apply() = new Transform

}
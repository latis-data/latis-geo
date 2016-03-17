package latis.ops

import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import org.geotools.referencing.crs.DefaultGeocentricCRS
import org.opengis.referencing.crs.CoordinateReferenceSystem

import com.vividsolutions.jts.geom.Coordinate

import latis.dm.Function
import latis.dm.Number
import latis.dm.Real
import latis.dm.Sample
import latis.dm.Tuple
import latis.dm.Variable
import latis.metadata.Metadata
import latis.util.iterator.MappingIterator

/**
 * Transform from ECEF to WGS84 3D.
 */
class Transform extends Operation {
    
  val sourceCRS: CoordinateReferenceSystem = DefaultGeocentricCRS.CARTESIAN //EPSG:4978
  val targetCRS: CoordinateReferenceSystem = CRS.decode("epsg:4979") //WGS84 3D
  
  lazy val transform = CRS.findMathTransform(sourceCRS, targetCRS)
  
  //Note, applies to any tuple with 3 Numbers.
  //TODO: match name or metadata convention for geolocation
  override def applyToTuple(tuple: Tuple) = tuple match {
    case Tuple(Seq(Number(x), Number(y), Number(z))) => {
      
      //println(CRS.getAxisOrder(sourceCRS)) //INAPPLICABLE
      //println(CRS.getAxisOrder(targetCRS)) //NORTH_EAST
      
      val coord = new Coordinate(x,y,z)
      val tcoord = JTS.transform(coord, null, transform)
      val tup = Tuple(Real(Metadata("latitude"),  tcoord.x), 
                      Real(Metadata("longitude"), tcoord.y), 
                      Real(Metadata("altitude"),  tcoord.z))
      Some(tup)
    }
    case _ => super.applyToTuple(tuple) //presumably some other Tuple in the dataset
      //throw new UnsupportedOperationException("The Transform Operation expects a Tuple(x,y,z).")
  }
  
//  override def applyToSample(sample: Sample): Option[Sample] = {
//    val coord = sample.range match {
//      case Tuple(Seq(Number(x), Number(y), Number(z))) => new Coordinate(x,y,z)
//      case _ => throw new UnsupportedOperationException(
//          "A function must have a range of the form Tuple(x,y,z) to be transformed")
//    }
//    
//    val tcoord = JTS.transform(coord, null, transform)
//    
//    val ran = Tuple(Real(Metadata("latitude"), tcoord.x), 
//                    Real(Metadata("longitude"), tcoord.y), 
//                    Real(Metadata("altitude"), tcoord.z))
//    
//    Some(Sample(sample.domain, ran))
//  }
  
  /**
   * Override to set the new CRS Metadata.
   */
  override def applyToFunction(function: Function): Option[Variable] = {
    super.applyToFunction(function) match {
      case Some(f: Function) => Some(Function(f.getDomain, f.getRange, f.iterator, f.getMetadata + ("epsg" -> "4979")))
      case other => other
    }
  }
  
}

object Transform extends OperationFactory {
  
  override def apply() = new Transform
  
}
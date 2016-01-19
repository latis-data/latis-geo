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
  
  override def applyToSample(sample: Sample): Option[Sample] = {
    val coord = sample.domain match {
      case Tuple(Seq(Number(x), Number(y), Number(z))) => new Coordinate(x,y,z)
      case _ => throw new UnsupportedOperationException(
          "A function must have a domain of the form Tuple(x,y,z) to be transformed")
    }
    
    JTS.transform(coord, coord, transform)
    
    val dom = Tuple(Real(Metadata("lat"), coord.x), 
                    Real(Metadata("lon"), coord.y), 
                    Real(Metadata("alt"), coord.z))
    
    Some(Sample(dom, sample.range))
  }
  
  /**
   * Override to set the new CRS Metadata.
   */
  override def applyToFunction(function: Function): Option[Variable] = {
    val mit = new MappingIterator(function.iterator, (s: Sample) => this.applyToSample(s))
    val template = mit.peek match {
      case null => function.getSample //empty iterator so no-op
      case s => s
    }
    
    val md = function.getMetadata + ("epsg" -> "4979")
    
    Some(Function(template.domain, template.range, mit, md))
  }
  
}

object Transform extends OperationFactory {
  
  override def apply() = new Transform
  
}
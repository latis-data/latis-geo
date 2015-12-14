package latis.ops

import latis.dm._
import com.vividsolutions.jts.geom.Coordinate
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import latis.util.iterator.MappingIterator
import scala.collection.JavaConversions._
import org.geotools.referencing.crs.DefaultGeographicCRS

class Transform(var sourceCRS: CoordinateReferenceSystem, 
    targetCRS: CoordinateReferenceSystem) extends Operation {
    
  lazy val transform = CRS.findMathTransform(sourceCRS, targetCRS)
  
  override def applyToSample(sample: Sample): Option[Sample] = {
    val coord = sample.domain match {
      case Tuple(Seq(Number(x), Number(y))) => new Coordinate(x,y)
      case Tuple(Seq(Number(x), Number(y), Number(z))) => new Coordinate(x,y,z)
      case _ => throw new UnsupportedOperationException(
          "A function must have a domain of the form Tuple(x,y) or Tuple(x,y,z) to be transformed")
    }
    
    JTS.transform(coord, coord, transform)
    
    val dom = if(coord.z.isNaN) Tuple(Real(coord.x), Real(coord.y))
      else Tuple(Real(coord.x), Real(coord.y), Real(coord.z))
    
    Some(Sample(dom, sample.range))
  }
  
  /**
   * Override to get the source CRS and set the new CRS Metadata.
   */
  override def applyToFunction(function: Function): Option[Variable] = {
    function.getMetadata("epsg") match {
      case Some(s) => sourceCRS = CRS.decode(s"epsg:$s")
      case None =>
    }
    
    val mit = new MappingIterator(function.iterator, (s: Sample) => this.applyToSample(s))
    val template = mit.peek match {
      case null => function.getSample //empty iterator so no-op
      case s => s
    }
    
    val md = targetCRS.getIdentifiers.find(_.getCodeSpace == "EPSG") match {
      case Some(id) => function.getMetadata + ("epsg", id.getCode)
      case None => function.getMetadata
    }
    
    Some(Function(template.domain, template.range, mit, md))
  }
  
}

object Transform extends OperationFactory {
  
  def apply(scode: String, tcode: String) = new Transform(CRS.decode(s"epsg:$scode"), CRS.decode(s"epsg:$tcode"))
  
  def apply(tcode: String) = new Transform(DefaultGeographicCRS.WGS84_3D, CRS.decode(s"epsg:$tcode"))
  
  override def apply(args: Seq[String]) = args.length match {
    case 1 => Transform(args.head)
    case 2 => Transform(args.head, args.last)
    case _ => ???
  }
  
}
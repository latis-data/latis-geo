package latis.reader.tsml

import java.awt.geom.Point2D
import java.awt.image.Raster
import latis.reader.tsml.ml.Tsml
import org.geotools.gce.geotiff.GeoTiffReader
import org.geotools.coverage.grid.GridCoverage2D
import org.opengis.referencing.operation.MathTransform2D
import latis.data.Data
import latis.metadata.Metadata
import latis.reader.tsml.ml.VariableMl
import latis.reader.tsml.ml.FunctionMl
import org.geotools.referencing.CRS
import com.typesafe.scalalogging.slf4j.Logging

class GeoTiffAdapter(tsml: Tsml) extends IterativeAdapter[((Double, Double), Array[Int])](tsml) with Logging{
  
  lazy val reader: GeoTiffReader = new GeoTiffReader(getUrl)
  
  lazy val coverage: GridCoverage2D = reader.read(null)
  
  lazy val transform: MathTransform2D = coverage.getGridGeometry.getGridToCRS2D
  lazy val raster: Raster = coverage.getRenderedImage.getData
  
  //will be stored as metadata in the function
  lazy val crs = coverage.getCoordinateReferenceSystem2D
  lazy val height = raster.getHeight
  lazy val width = raster.getWidth
  
  //indices used to access the raster data
  def indexes: Iterator[(Int, Int)] = {
    val x = (0 until width)
    val y = Iterator.range(0, height)
    for(j <- y; i <- x) yield(i, j)
  }
  
  //get values for a sample domain for the given indices
  val domain = (xy: (Int, Int)) => {
    val pt = new Point2D.Double(xy._1, xy._2)
    val dom = transform.transform(pt, pt)
    (dom.getX, dom.getY)
  }
  
  //get values for a sample range for the given indices
  val pixels = (xy: (Int, Int)) => {
    raster.getPixel(xy._1, xy._2, new Array[Int](raster.getNumBands))
  }
  
  def close = {
    reader.dispose
  }
  
  //An awkward record type, but it works
  override def getRecordIterator: Iterator[((Double, Double), Array[Int])] = {
    indexes.map(xy => (domain(xy), pixels(xy)))
  }
  
  override def parseRecord(record: ((Double, Double), Array[Int])): Option[Map[String, Data]] = {
    val names = getOrigScalarNames
    val data = recordToData(record)
    
    if(data.length != names.length) {
      logger.debug("Invalid record: " + data.length + " values found for " + names.length + " variables")
      None
    }
    else Option(names.zip(data).toMap)
  }
  
  def recordToData(rec: ((Double, Double), Array[Int])): Seq[Data] = {
    List(Data(rec._1._1), Data(rec._1._2)) ++ rec._2.map(i => Data(i))
  }
  
  
  /**
   * Adds metadata from tsml as well as the width, height, and coordinate reference system of the tiff
   */
  override protected def makeMetadata(vml: VariableMl): Metadata = vml match {
    case fml: FunctionMl => {
      val c = CRS.lookupEpsgCode(crs, true) match {
        case c: Integer => ("epsg" -> c.toString)
        case null => ("wkt" -> crs.toWKT)
      }
      val w = ("width" -> width.toString)
      val h = ("height" -> height.toString)
      super.makeMetadata(fml) + c + w + h
    }
    case _ => super.makeMetadata(vml)
  }
  
}
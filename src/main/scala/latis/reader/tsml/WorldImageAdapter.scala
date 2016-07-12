package latis.reader.tsml

import java.awt.geom.Point2D
import java.awt.image.Raster

import org.geotools.coverage.grid.GridCoverage2D
import org.geotools.coverage.grid.io.AbstractGridCoverage2DReader
import org.geotools.gce.image.WorldImageReader
import org.opengis.referencing.operation.MathTransform2D

import latis.data.Data
import latis.data.SampledData
import latis.dm.Function
import latis.reader.tsml.ml.Tsml
import latis.util.Crs

class WorldImageAdapter(tsml: Tsml) extends IterativeAdapter[((Double, Double), Array[Int])](tsml) {
    
  private var _reader: AbstractGridCoverage2DReader = null
  lazy val reader = {
    _reader = new WorldImageReader(getUrl)
    _reader
  }
  
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
  lazy val domain = (xy: (Int, Int)) => {
    val pt = new Point2D.Double(xy._1, xy._2)
    val dom = transform.transform(pt, pt)
    (dom.getX, dom.getY)
  }
  
  //get values for a sample range for the given indices
  lazy val pixels = (xy: (Int, Int)) => {
    raster.getPixel(xy._1, xy._2, new Array[Int](raster.getNumBands))
  }
  
  def close = {
    if (_reader != null) _reader.dispose
  }
  
  //An awkward record type, but it works
  override def getRecordIterator: Iterator[((Double, Double), Array[Int])] = {
    indexes.map(xy => (domain(xy), pixels(xy)))
  }
  
  override def parseRecord(record: ((Double, Double), Array[Int])): Option[Map[String, Data]] = {
    val names = getOrigScalarNames
    val data = recordToData(record)
    
    if(data.length != names.length) None
    else Option(names.zip(data).toMap)
  }
  
  def recordToData(rec: ((Double, Double), Array[Int])): Seq[Data] = {
    List(Data(rec._1._1), Data(rec._1._2)) ++ rec._2.map(i => Data(i))
  }
  
  /**
   * Override so we can add width, height, and coordinate reference system metadata.
   */
  override def makeFunction(f: Function): Option[Function] = {
    //Delegate to super to make the Function
    val f2 = super.makeFunction(f).get
    
    //Add metadata
    val c = Crs.lookupEpsgCode(crs, true) match {
      case c: Integer => ("crs" -> s"EPSG:${c.toString}")
      case null => ("wkt" -> crs.toWKT)
    }
    val w = ("width" -> width.toString)
    val h = ("height" -> height.toString)
    val layerType = ("layerType" -> "image")
    val md = f.getMetadata + c + w + h + layerType
    Some(Function(f2.getDomain, f2.getRange, md, f2.getData.asInstanceOf[SampledData]))
  }
}
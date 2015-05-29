package latis.writer

import java.awt.image.DataBuffer
import java.awt.image.PixelInterleavedSampleModel
import java.awt.image.Raster
import java.awt.image.WritableRaster
import java.io.File
import org.geotools.coverage.CoverageFactoryFinder
import org.geotools.geometry.jts.ReferencedEnvelope
import org.geotools.referencing.CRS
import org.opengis.geometry.Envelope
import latis.dm.Dataset
import latis.dm.Function
import latis.dm.Tuple
import org.geotools.coverage.grid.GridCoverage2D
import latis.dm.Scalar
import latis.dm.Sample
import latis.util.iterator.PeekIterator

class GeoTiffWriter extends FileWriter {
  
  /**
   * Creates a WritableRaster with a size determined by the function's metadata.
   */
  def getRaster(f: Function): WritableRaster = {
    val width = f.getMetadata("width").getOrElse(
        throw new Exception("function must have pixel width defined in the tsml")).toInt
    val height = f.getMetadata("height").getOrElse(
        throw new Exception("function must have pixel height defined in the tsml")).toInt
    val bands = f.getRange.toSeq.length
    
    val model = new PixelInterleavedSampleModel(DataBuffer.TYPE_BYTE, width, height, bands, width * bands, Array.range(0, bands))
    
    Raster.createWritableRaster(model, null)
  }
  
  /**
   * Reads the function data into a raster.
   * Returns the first and last samples of the function, 
   * which are used to align the image with a coordinate system.
   */
  def fillRaster(f: Function, raster: WritableRaster): (Sample, Sample) = {
    val xs = (0 until raster.getWidth)
    val ys = Iterator.range(0, raster.getHeight)
    val indexes = for(y <- ys; x <- xs) yield (x, y)
    val it = PeekIterator(f.iterator.zip(indexes))
    
    val first = it.peek._1
    
    var last = first
    for((s, (x, y)) <- it) {
      raster.setPixel(x, y, s.range.toSeq.map(_.getNumberData.intValue).toArray)
      last = s
    }
    (first, last)
  }
  
  /**
   * Makes an envelope which is used to describe the position of this image 
   * relative to a coordinate system defined in the metadata.
   */
  def makeEnvelope(k: (Sample, Sample), function: Function): Envelope = {
    val crs = function.getMetadata("epsg") match {
      case Some(s) => CRS.decode(s"epsg:$s")
      case None => function.getMetadata("wkt") match {
        case Some(s) => CRS.parseWKT(s)
        case None => throw new Exception("function must have a coordinate system defined using either epsg or wkt")
      }
    }
    
    val f = k._1.domain.toSeq.map(_.getNumberData.doubleValue)
    val l = k._2.domain.toSeq.map(_.getNumberData.doubleValue)
    
    new ReferencedEnvelope(f(0), l(0), f(1), l(1), crs)
  }
  
  /**
   * Maps the image raster to the coordinate system.
   */
  def makeCoverage(f: Function) = {
    val raster = getRaster(f)
    val envelope = makeEnvelope(fillRaster(f, raster), f)
    val gcf = CoverageFactoryFinder.getGridCoverageFactory(null)
    gcf.create("grid", raster, envelope)
  }
  
  def writeFile(ds: Dataset, file: File) = {
    val f = ds match {
      case Dataset(f: Function) => f
    }
    
    val coverage = makeCoverage(f)
    
    val writer = new org.geotools.gce.geotiff.GeoTiffWriter(file)
    writer.write(coverage, null)
    writer.dispose
    coverage.dispose(true)
  }
  
  override def mimeType = "image/tif"

}
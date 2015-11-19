package latis.writer

import java.awt.image.BufferedImage
import java.awt.image.DataBuffer
import java.awt.image.PixelInterleavedSampleModel
import java.awt.image.Raster
import java.io.File

import org.geotools.coverage.CoverageFactoryFinder
import org.geotools.coverage.grid.GridCoverage2D
import org.geotools.geometry.jts.ReferencedEnvelope
import org.geotools.referencing.CRS
import org.opengis.geometry.Envelope

import latis.dm.Dataset
import latis.dm.Function
import latis.dm.Sample
import latis.util.ColorModels
import latis.util.iterator.PeekIterator

class GeoTiffWriter extends FileWriter {
  
  protected var function: Function = null
  /**
   * Creates a WritableRaster with a size determined by the function's metadata.
   */
  protected lazy val raster = {
    val width = function.getMetadata("width").getOrElse(
        throw new Exception("function must have pixel width defined in the tsml")).toInt
    val height = function.getMetadata("height").getOrElse(
        throw new Exception("function must have pixel height defined in the tsml")).toInt
    val bands = function.getRange.toSeq.length
    
    val model = new PixelInterleavedSampleModel(DataBuffer.TYPE_BYTE, width, height, bands, width * bands, Array.range(0, bands))
    
    Raster.createWritableRaster(model, null)
  }
  
  /**
   * Reads the function data into a raster.
   * Returns the first and last samples of the function, 
   * which are used to align the image with a coordinate system.
   */
  protected lazy val bounds: (Sample, Sample) = {
    val xs = (0 until raster.getWidth)
    val ys = Iterator.range(0, raster.getHeight)
    val indexes = for(y <- ys; x <- xs) yield (x, y)
    val it = PeekIterator(function.iterator.zip(indexes))
    
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
  protected lazy val envelope: Envelope = {
    val crs = function.getMetadata("epsg") match {
      case Some(s) => CRS.decode(s"epsg:$s")
      case None => function.getMetadata("wkt") match {
        case Some(s) => CRS.parseWKT(s)
        case None => throw new Exception("function must have a coordinate system defined using either epsg or wkt")
      }
    }
    
    val f = bounds._1.domain.toSeq.map(_.getNumberData.doubleValue)
    val l = bounds._2.domain.toSeq.map(_.getNumberData.doubleValue)
    
    new ReferencedEnvelope(f(0), l(0), f(1), l(1), crs)
  }
  
  /**
   * Maps the image raster to the coordinate system.
   * Also applies a color map if it is specified in the tsml.
   */
  protected lazy val coverage: GridCoverage2D = {
    val gcf = CoverageFactoryFinder.getGridCoverageFactory(null)
    function.getMetadata("color_map") match {
      case None => gcf.create("grid", raster, envelope)
      case Some(name) => gcf.create("grid", new BufferedImage(ColorModels(name), raster, true, null), envelope)
    }
  }
  
  def writeFile(ds: Dataset, file: File) = {
    val f = ds match {
      case Dataset(f: Function) => f
    }
    
    function = f
    
    val writer = new org.geotools.gce.geotiff.GeoTiffWriter(file)
    writer.write(coverage, null)
    writer.dispose
    coverage.dispose(true)
  }
  
  override def mimeType = "image/tif"

}
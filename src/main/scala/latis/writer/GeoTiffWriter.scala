package latis.writer

import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.awt.image.DataBuffer
import java.awt.image.PixelInterleavedSampleModel
import java.awt.image.Raster
import java.awt.image.WritableRaster

import scala.collection.JavaConversions.bufferAsJavaList
import scala.collection.mutable.ListBuffer

import org.geotools.coverage.CoverageFactoryFinder
import org.geotools.coverage.grid.GridCoverage2D
import org.geotools.data.DataUtilities
import org.geotools.data.collection.ListFeatureCollection
import org.geotools.data.simple.SimpleFeatureCollection
import org.geotools.factory.CommonFactoryFinder
import org.geotools.factory.Hints
import org.geotools.feature.simple.SimpleFeatureBuilder
import org.geotools.geometry.jts.JTSFactoryFinder
import org.geotools.geometry.jts.ReferencedEnvelope
import org.geotools.map.FeatureLayer
import org.geotools.map.GridCoverageLayer
import org.geotools.map.Layer
import org.geotools.map.MapContent
import org.geotools.referencing.CRS
import org.geotools.renderer.lite.StreamingRenderer
import org.geotools.styling.SLD
import org.geotools.styling.Style
import org.opengis.feature.simple.SimpleFeature
import org.opengis.geometry.Envelope
import org.opengis.referencing.crs.CoordinateReferenceSystem
import org.opengis.referencing.cs.AxisDirection.EAST
import org.opengis.referencing.cs.AxisDirection.NORTH

import com.vividsolutions.jts.geom.Coordinate

import latis.dm.Dataset
import latis.dm.Function
import latis.dm.Number
import latis.dm.Sample
import latis.dm.Tuple
import latis.dm.Real
import latis.util.ColorModels
import latis.util.iterator.PeekIterator
import latis.util.CircleMarkPointStyle
import latis.util.WindMarkPointStyle
import org.geotools.feature.FeatureCollection
import org.geotools.factory.Hints
import org.geotools.referencing.ReferencingFactoryFinder

/**
 * Uses Geotools to write a Geotiff image. The Dataset to be written must be modeled 
 * as either a single gridded Function or a Tuple of Functions. In the second case, each Function
 * will be treated as an individual layer of the image. 
 * 
 * Gridded Functions (rasters) must have a regular Cartesian product as a domain.
 * For a grayscale image, the range should be a single Scalar. For colored images, the 
 * range should be a Tuple containing a Scalar for the blue, green, and red bands. 
 * 
 * Feature Functions (lines, points) must be named "line" or "points" respectively 
 * and have a range consisting of a Tuple of longitude and latitude values.
 *  
 * All Functions can have a coordinate reference system specified by using an EPSG code 
 * in the Metadata and will default to the code 4326 if no "epsg" Metadata is specified. 
 */
class GeoTiffWriter extends Writer {
  
  Hints.putSystemDefault(Hints.FORCE_LONGITUDE_FIRST_AXIS_ORDER, true)
  
  /**
   * Get the CoordinateReferenceSystem defined by epsg code
   * in this Function's Metadata. If no "epsg" metadata is defined,
   * default to WGS84 (epsg:4326).
   */
  def getCrs(function: Function): CoordinateReferenceSystem = {
    function.getMetadata("epsg") match {
      case Some("404000") => CRS.decode("epsg:4326") //404000 is an unusable default when reading images
      case Some(s) => CRS.decode(s"epsg:$s")
      case None => CRS.decode("epsg:4326")
    }
  }
  
  def getLatLon(s: Sample): (Double, Double) = (s.findVariableByName("latitude"), s.findVariableByName("longitude")) match {
    case (Some(Number(lat)), Some(Number(lon))) => (lat,lon)
    case _ => throw new Exception("Sample did not contain variables named 'latitude and 'longitude'.")
  }
  
  /**
   * Get the width, height, and number of variables in the range of the given function. 
   */
  def getDimensions(function: Function): (Int, Int, Int) = {
    val samples = function.iterator.toSeq 
    val length = samples.size
    
    val (lats, lons) = samples.map(getLatLon).unzip
    
    val width = lons.distinct.size
    
    val height = lats.distinct.size
    
    val bands = samples.head match {
      case Sample(_, n: Number) => 1
      case Sample(_, Tuple(Seq(b: Number, g: Number, r: Number))) => 3
      case _ => throw new Exception("Images can only be made of Functions with 1 or 3 bands in the range.")
    }
    
    if(width * height != length) throw new Exception("Function domain must be represented by a Cartesian product set.")
    
    (width, height, bands)
  }
  
  /**
   * Creates a WritableRaster with a size determined by the function's metadata.
   */
  def getRaster(function: Function): WritableRaster = {
    val (width, height, bands) = getDimensions(function)
    
    val model = new PixelInterleavedSampleModel(DataBuffer.TYPE_BYTE, width, height, bands, width * bands, Array.range(0, bands))
    
    Raster.createWritableRaster(model, null)
  }
  
  /**
   * Reads the function data into a raster.
   * Returns the first and last samples of the function, 
   * which are used to align the image with a coordinate system.
   */
  private def fillRaster(function: Function, raster: WritableRaster): (Sample, Sample) = {
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
  def getEnvelope(function: Function, raster: WritableRaster): Envelope = {
    val crs = getCrs(function)
    
    val bounds = fillRaster(function, raster)
    
    val (lat1, lon1) = getLatLon(bounds._1)
    val (lat2, lon2) = getLatLon(bounds._2)
    
    //Get the lon/lat bounding box
    //Xs then Ys of the image dataset
    val minLon = Math.min(lon1, lon2)
    val maxLon = Math.max(lon1, lon2)
    val minLat = Math.min(lat1, lat2)
    val maxLat = Math.max(lat1, lat2)
    new ReferencedEnvelope(minLon, maxLon, minLat, maxLat, crs)
  }
  
  /**
   * Create a buffered image of this function. 
   * If a color map is defined for this function, it is applied here.
   */
  def getImage(function: Function, raster: WritableRaster): BufferedImage = {
    function.getMetadata("color_map") match {
      case None => {
        raster.getNumBands match {
          case 1 => {
            val image = new BufferedImage(raster.getWidth, raster.getHeight, BufferedImage.TYPE_BYTE_GRAY)
            image.setData(raster)
            image
          }
          case 3 => {
            val image = new BufferedImage(raster.getWidth, raster.getHeight, BufferedImage.TYPE_3BYTE_BGR)
            image.setData(raster)
            image
          }
        }
      }
      case Some(name) => new BufferedImage(ColorModels(name), raster, false, null)
    }
  }
  
  /**
   * Maps the image to the coordinate system.
   */
  def getCoverage(function: Function): GridCoverage2D = {
    val gcf = CoverageFactoryFinder.getGridCoverageFactory(null)
    val raster = getRaster(function)
    val envelope = getEnvelope(function, raster)
    val image = getImage(function, raster)
    gcf.create("grid", image, envelope)
  }
  
  /**
   * Gets the default style of the appropriate type for the given function.
   */
  def getStyle(function: Function): Style = {
    val sf = CommonFactoryFinder.getStyleFactory
    val sym = if(function.hasName("line")) sf.getDefaultLineSymbolizer
      else if(function.hasName("points")) CircleMarkPointStyle.getCustomPointCircleSymbolizer(sf)
      else sf.getDefaultRasterSymbolizer
    SLD.wrapSymbolizers(sym)
  }
  
  /**
   * Makes a FeatureCollection containing a single line.
   */
  def getLineCollection(function: Function): SimpleFeatureCollection = {
    val crs = getCrs(function)
    val cs = crs.getCoordinateSystem
    
    val srid = function.getMetadata("epsg") match {
      case Some("4979") => "4326" //make it 2D
      case Some(s) => s
      case None => "4326"
    }
    
    val ftype = DataUtilities.createType("line", s"line:LineString:srid=$srid")
    val fcol = ListBuffer[SimpleFeature]()
    val fbuilder = new SimpleFeatureBuilder(ftype)
    val gfac = JTSFactoryFinder.getGeometryFactory
    
    val orderAxes = (cs.getAxis(0).getDirection, cs.getAxis(1).getDirection) match {
      case (EAST, NORTH) => (lat: Double, lon: Double) => new Coordinate(lon, lat)
      case (NORTH, EAST) => (lat: Double, lon: Double) => new Coordinate(lat, lon)
    }
    
    val coords = function.iterator.map(s => {
      val (lat, lon) = getLatLon(s)
      orderAxes(lat,lon)
    })
    
    val line = gfac.createLineString(coords.toArray)
    fbuilder.add(line)
    val f = fbuilder.buildFeature(null)
    fcol += f
    new ListFeatureCollection(ftype, fcol)
  }
  
  /**
   * Makes a FeatureCollection containing a Point for each 
   * sample in the function.
   */
  def getPointCollection(function: Function): SimpleFeatureCollection = {
    val crs = getCrs(function)
    val cs = crs.getCoordinateSystem
    
    val srid = function.getMetadata("epsg") match {
      case Some("4979") => "4326" //make it 2D
      case Some(s) => s
      case None => "4326"
    }
    
    val ftype = DataUtilities.createType("point", s"point:Point:srid=$srid")
    val fcol = ListBuffer[SimpleFeature]()
    val fbuilder = new SimpleFeatureBuilder(ftype)
    val gfac = JTSFactoryFinder.getGeometryFactory
    
    val orderAxes = (cs.getAxis(0).getDirection, cs.getAxis(1).getDirection) match {
      case (EAST, NORTH) => (lat: Double, lon: Double) => new Coordinate(lon, lat)
      case (NORTH, EAST) => (lat: Double, lon: Double) => new Coordinate(lat, lon)
    }
    
    val coords = function.iterator.map(s => {
      val (lat, lon) = getLatLon(s)
      orderAxes(lat,lon)
    })
    
    coords.foreach { c => 
      val point = gfac.createPoint(c)
      fbuilder.add(point)
      val f = fbuilder.buildFeature(null)
      fcol += f
    }
    new ListFeatureCollection(ftype, fcol)
  }
  
  /**
   * Constructs a layer using a coverage and a style.
   */
  def getLayer(function: Function): Layer = {
    if(function.hasName("line")) {
      val fcol = getLineCollection(function)
      val style = getStyle(function)
      new FeatureLayer(fcol, style)
    } else if(function.hasName("points")) {
      val fcol = getPointCollection(function)
      val style = getStyle(function)
      new FeatureLayer(fcol, style)
    } else {
      val coverage = getCoverage(function)
      val style = getStyle(function)
      new GridCoverageLayer(coverage, style)
    }
  }
  
  def getWindAngle(u: Double, v: Double): Double = {
    // normalizing u & v
    val norm = Math.sqrt(u*u + v*v)
    // wind angle
    val atan = Math.atan2(u/norm, v/norm)
    // to degrees
    val deg = atan * 180/Math.PI
    // get clockwise angle from y-axis
    val angle = 90 - deg
    angle
  }
  
  def getWinArrowLayers(function: Function): Iterator[Layer] = {
    // build a layer for each sample in the wind function
    function.iterator.map { x => 
      val (lon,lat) = (x.findVariableByName("longitude"), x.findVariableByName("latitude")) match {
        case (Some(Real(lat)), Some(Real(lon))) => (lat,lon)
        }
      val (u,v) = (x.range.findVariableByName("u10m"),x.range.findVariableByName("v10m")) match {
        case (Some(Real(u)), Some(Real(v))) => (u,v)
        }
      val angle = getWindAngle(u,v)
          
      val ftype = DataUtilities.createType("point", "point:Point:srid=4326")
      val fbuilder = new SimpleFeatureBuilder(ftype)
      val gfac = JTSFactoryFinder.getGeometryFactory
      val point = gfac.createPoint(new Coordinate(lon, lat))
      fbuilder.add(point)
      val f = fbuilder.buildFeature(null)
      val fcol = new ListFeatureCollection(ftype, ListBuffer(f))
          
      val sf = CommonFactoryFinder.getStyleFactory
      val arrow = WindMarkPointStyle.getCustomWindSymbolizer(sf,angle)
      val style = SLD.wrapSymbolizers(arrow)
          
      val layer = new FeatureLayer(fcol, style)
      layer
    }
  }

  /**
   * Map each function in a dataset to a layer in a MapContent. 
   */
  def getMap(ds: Dataset): MapContent = {
    /*
    val layers = ds match {
      case Dataset(f: Function) => Seq(getLayer(f))
      case Dataset(Tuple(vs)) => vs.flatMap(v => v match {
        case f: Function => Some(getLayer(f))
        case _ => None
      })
    }
    * 
    */
    
    val map = new MapContent()
    map.setTitle(ds.getName)
    
    // layer order is important!!
    val imageLayer = ds match {
      case Dataset(Tuple(t)) => t(1) match {
        case f: Function => getLayer(f)
      }
    }
    map.addLayer(imageLayer)
    
    val eventLayer = ds match {
      case Dataset(Tuple(t)) => t(2) match {
        case f: Function => getLayer(f)
      }
    }
    map.addLayer(eventLayer)
    
   val windfunc = ds match {
     case Dataset(Tuple(t)) => t(0) match {
       case f: Function => f
       }
     }
    
   val windLayer = getWinArrowLayers(windfunc)
   windLayer.foreach(map.addLayer(_))
   
    map
  }
  
  /**
   * Construct a MapContent and paint that onto a new image. 
   * Then write the image to the specified file. 
   */
  def write(dataset: Dataset) = {
    val ds = dataset.force
    val map = getMap(ds)
    
    //the first function must have gridded data with an appropriate width and height
    //ugh, is there a way to ask the dataset to give us the function named "image" instead of doing this?
    val f2 = ds match {
      case Dataset(t) => t match {
        case Tuple(f) => f(1) match {
          case i: Function => i
        }
      }
    }
    //val f = ds.unwrap.findFunction.get
    val (width, height, bands) = getDimensions(f2)
    
    //create the image that the dataset will be written to.
    val image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)
    val gr = image.createGraphics
    
    val mapBounds = map.getViewport.getBounds
    val imgBounds = new Rectangle(width, height)
    
    //use a renderer to paint the map onto the image
    val renderer = new StreamingRenderer()
    renderer.setMapContent(map)
    renderer.paint(gr, imgBounds, mapBounds)
    
    //make a coverage of the image so that it can be written as geotiff
    val gcf = CoverageFactoryFinder.getGridCoverageFactory(null)
    val coverage = gcf.create(ds.getName, image, mapBounds)
    
    //write the coverage
    val writer = new org.geotools.gce.geotiff.GeoTiffWriter(getOutputStream)
    writer.write(coverage, null)
    writer.dispose
    coverage.dispose(true)
    map.dispose()
  }
  
  override def mimeType = "image/tif"

}
package latis.reader

import latis.dm._
import latis.ops._
import latis.ops.filter._
import latis.writer.Writer
import java.io.File
import com.typesafe.scalalogging.LazyLogging
import latis.time.Time
import latis.ops.agg.ImageTileAggregation
import latis.util.LatisProperties

/**
 * Create a geo-referenced image dataset for WMTS tiles.
 *   (longitude, latitude) -> (band1, band2, band3)
 * where the bands are typically (Red, Green, Blue).
 * We are not currently modeling time. A single time selection is required since
 * this can't model a time series, yet. (NOMS-94) "longitude" and "latitude" selections
 * are optional.
 * 
 * This reader depends on a "wmts_tiles" dataset that returns a set of URLs, one
 * for each tile image:
 *   time -> index -> (minLon, maxLon, minLat, maxLat, file)
 *   
 * If a "level" selection is provided, all tiles for the requested time and region
 * and given zoom level will be joined into a single "image" (so this may need a lot of memory).
 * If no "level" selection is provided, the "wmts_tiles" dataset will return a
 * single "best fit" tile from the highest resolution level with a tile that
 * contains the requested region.
 * 
 * Images will be transformed from the (row, column) domain to (longitude, latitude).
 * The order of the pixels is not modified.
 */
class WmtsImageReader extends DatasetAccessor with LazyLogging {
  
  //Keep readers at global scope so we can close them.
  private var tileListReader: DatasetAccessor = null
  private var imageReader: DatasetAccessor = null
  
  private var baseUrl: String = null
  
  def getDataset(operations: Seq[Operation]): Dataset = {
    // Get zoom level from latis.properties
    val zoomLevel = getProperty("level") match {
      case Some(s) => s.toInt
      case None => -1
    }
    logger.debug("WMTS tiles zoom level set to: " + zoomLevel);
    val ops = operations :+ Selection("level=" + zoomLevel)
    
    //Get the dataset of tiles for the given ops.
    //  (time -> index -> (minLon, maxLon, minLat, maxLat, file))
    tileListReader = DatasetAccessor.fromName("wmts_tiles")
    val tileds = tileListReader.getDataset(ops)

    //get the base URL for the tile data from the wmts dataset metadata
    baseUrl = tileds.getMetadata.get("baseUrl") match {
      case Some(s) => s
      case None => throw new Error("No baseUrl defined in the tile dataset.")
    }
    
    //Extract the info to read the image and transform it from (row,column) to (longitude,latitude)
    val geods = tilesToDataset(tileds)
    
    //Apply spatial selections to the geo-referenced image.
    //TODO: apply to each tile first?
    operations.filter(isSpatialSelection(_)).foldLeft(geods)((ds, op) => op(ds))
  }

  /**
   * Given a dataset of WMTS tile definitions:
   *   time -> index -> (minLon, maxLon, minLat, maxLat, file)
   * create a dataset for each tile then join them together as a single image:
   *   (longitude, latitude) -> (band1, band2, band3)
   */
  private def tilesToDataset(tileDataset: Dataset): Dataset = tileDataset match {
    case Dataset(Function(it)) => it.next match { //assuming one time sample
    //TODO: deal with empty tileDataset
    case Sample(_, Function(it2)) => {
        val tiles = it2.toSeq.map(smp => smp match {
          case Sample(_, tile: Tuple) => tileToGeoImage(tile)
        })
        ImageTileAggregation()(tiles)
      }
    }
  }

  /**
   * Given a Tuple with a single tile definition from the wmts_tiles Dataset:
   *   (minLon, maxLon, minLat, maxLat, file)
   * create an image dataset and geo-reference it:
   *   (longitude, latitude) -> (band1, band2, band3)
   */
  private def tileToGeoImage(tile: Tuple): Dataset = {
    //TODO: general utility method?
    val minLon = tile.findVariableByName("minLon") match { case Some(Number(d)) => d }
    val maxLon = tile.findVariableByName("maxLon") match { case Some(Number(d)) => d }
    val minLat = tile.findVariableByName("minLat") match { case Some(Number(d)) => d }
    val maxLat = tile.findVariableByName("maxLat") match { case Some(Number(d)) => d }
    val file = tile.findVariableByName("file") match { case Some(Text(s)) => s }

    val url = if (baseUrl.endsWith(File.separator)) baseUrl + file
    else baseUrl + File.separator + file
    logger.debug(s"Reading image: " + url)
    imageReader = ImageReader(url)
    val imageds = imageReader.getDataset()
    RowColToLonLat(minLon, maxLon, minLat, maxLat)(imageds)
  }
  
  private def isSpatialSelection(op: Operation) : Boolean = op match {
    case Selection(v,_,_) if (v == "longitude" || v == "latitude") => true
    case _ => false
  }
  
  def close = {
    if (tileListReader != null) tileListReader.close
    if (imageReader != null) imageReader.close
  }
}

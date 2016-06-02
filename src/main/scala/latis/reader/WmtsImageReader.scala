package latis.reader

import latis.dm._
import latis.ops._
import latis.ops.filter._
import latis.writer.Writer
import java.io.File
import com.typesafe.scalalogging.LazyLogging
import latis.time.Time

/**
 * Find the image for the given selection.
 * Use the tile information in the image URL and the WMTSCapabilities.xml
 * to transform the (row, column) domain to (longitude, latitude).
 * 
 */
class WmtsImageReader extends DatasetAccessor with LazyLogging {
  
  //Keep readers at global scope so we can close them.
  private var tileListReader: DatasetAccessor = null
  private var imageReader: DatasetAccessor = null
  
  def getDataset(operations: Seq[Operation]): Dataset = {
    //Get the dataset of tiles for the given ops; 
    //  requires a single time selection //TODO: enforce
    //  longitude, latitude optional
    //This will return one tile URL based on the requested lon,lat region (WmtsTileUrlGenerator).
    // time -> (minLon,maxLon,minLat,maxLat,file)
    //TODO: we now return a seq of files that need to be joined
    //  (time -> index -> (minLon, maxLon, minLat, maxLat, file))
    tileListReader = DatasetAccessor.fromName("wmts_tiles")
    val tileds = tileListReader.getDataset(operations)

    //Extract the info to read the image and transform it from (row,column) to (longitude,latitude)
    val geods = tilesToDataset(tileds)
    //apply spatial selections to the geo-referenced image
    operations.filter(isSpatialSelection(_)).foldLeft(geods)((ds, op) => op(ds))
  }

  private def tilesToDataset(tileDataset: Dataset): Dataset = tileDataset match {
    case Dataset(Function(it)) => it.next match {
      case Sample(_, Function(it2)) => it2.next match {
        case Sample(_, tup: Tuple) => {
          val minLon = tup.findVariableByName("minLon") match { case Some(Number(d)) => d }
          val maxLon = tup.findVariableByName("maxLon") match { case Some(Number(d)) => d }
          val minLat = tup.findVariableByName("minLat") match { case Some(Number(d)) => d }
          val maxLat = tup.findVariableByName("maxLat") match { case Some(Number(d)) => d }
          val file = tup.findVariableByName("file") match { case Some(Text(s)) => s }
          val baseUrl = tileDataset.getMetadata.get("baseUrl") match {
            case Some(s) => s
            case None => throw new Error("No baseUrl defined in the tile dataset.")
          }

          val url = if (baseUrl.endsWith(File.separator)) baseUrl + file
          else baseUrl + File.separator + file
          logger.debug(s"Reading image: " + url)
          imageReader = ImageReader(url)
          val imageds = imageReader.getDataset()
          RowColToLonLat(minLon, maxLon, minLat, maxLat)(imageds)
        }
      }
    }
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
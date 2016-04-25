package latis.util

import scala.Double
import scala.collection.mutable.ArrayBuffer

import latis.dm.Dataset
import latis.dm.Function
import latis.dm.Integer
import latis.dm.Sample
import latis.ops.Operation
import latis.ops.filter.LastFilter
import latis.ops.filter.Selection
import latis.reader.DatasetAccessor
import latis.util.GeoUtils.GeoBoundingBox

object WmtsUtils {

  private def tileMatrixDatasetSampleToTileMatrix(s: Sample): TileMatrix = {
    val id = s.findVariableByName("Identifier")   match { case Some(Integer(l)) => l.toInt }
    val tw = s.findVariableByName("TileWidth")    match { case Some(Integer(l)) => l.toInt }
    val th = s.findVariableByName("TileHeight")   match { case Some(Integer(l)) => l.toInt }
    val mw = s.findVariableByName("MatrixWidth")  match { case Some(Integer(l)) => l.toInt }
    val mh = s.findVariableByName("MatrixHeight") match { case Some(Integer(l)) => l.toInt }
    TileMatrix(id, tw, th, mw, mh)
  }
  

  //=========================================================================//
  
  case class TileMatrixSet(val id: String, matrices: Seq[TileMatrix]) {
    //TODO: until we model the entire capabilities dataset, assume this will be used only for EPSG4326_250m
    //for which gibs_epsg4326_250m_tile_matrix_set is defined and cached in the companion object
    
    def getTileMatrix(level: Int) = matrices(level)
    //TODO: make sure they are in order
        
    /**
     * Return the TileMatrix with the zoom level that has a tile that will completely
     * contain our area of interest.
     */
    def getBestTileMatrix(bbox: GeoBoundingBox): TileMatrix = {
      val tileMatrixSet = TileMatrixSet.fromIdentifier("EPSG4326_250m")
      
      //pick first level to try based on size of region
      val tileMatrix = getNominalTileMatrix(bbox)

      def recursiveHelper(box: GeoBoundingBox, level: Int): TileMatrix = {
        if (level < 0) throw new Error("There is no single tile that constains the requested region: " + bbox)
        //get matrix for level from tile matrix set
        val matrix = tileMatrixSet.getTileMatrix(level)
        val tiles = matrix.getTiles(box)
        if (tiles.length > 1) recursiveHelper(box, level - 1)
        else matrix
      }

      recursiveHelper(bbox, tileMatrix.level)
    }
    
    /**
     * Find the highest resolution tile matrix that has tiles large enough
     * to contain our area of interest based on the width and height of the
     * requested region in degrees. This uses a LaTiS dataset that models the tile
     * matrix sets defined in the WMTS' WMTSCapabilities.xml.
     * This tile matrix is "nominal" in that you may need a lower resolution tile
     * with a larger coverage if the region overlaps multiple tiles of the nominal
     * resolution.
     * Returns the TileMatrix identifier, i.e. zoom level, where 0 is the full
     * globe.
     */
    def getNominalTileMatrix(box: GeoBoundingBox): TileMatrix = {
      //get size of bounding box in degrees
      val dlon = box.nw.longitude - box.se.longitude
      val dlat = box.nw.latitude - box.se.latitude

      //request the tile matrix with tiles big enough to contain the box
      val ops = ArrayBuffer[Operation]()
      ops += Selection("tile_width_deg>" + dlon)
      ops += Selection("tile_height_deg>" + dlat)
      ops += LastFilter()
      //ops += Projection("Identifier")
      val ds = ops.foldLeft(TileMatrixSet.dataset)((ds, op) => op(ds))

      ds match {
        case Dataset(Function(it)) => tileMatrixDatasetSampleToTileMatrix(it.next)
      }
    }

  }
  
  object TileMatrixSet {
//    private lazy val tileMatrixSet = TileMatrixSet.fromIdentifier("EPSG4326_250m")
    
    //def fromLayer(layer: String) e.g. MODIS_Terra_CorrectedReflectance_TrueColor
    //get TileMatrixSet id from layers dataset
    //val ops = Seq(Selection("Title=" + dsName), Projection("TileMatrixSet"))
    //DatasetAccessor.fromName("gibs_layers").getDataset(ops).flatten match {
    //  case Dataset(Text(s)) => s
    //}
    
    def fromIdentifier(id: String = "EPSG4326_250m"): TileMatrixSet = {
      //TODO: support any id, currently hard-coded to do EPSG4326_250m
      val tileMatrices: Seq[TileMatrix] = dataset match {
        case Dataset(Function(it)) => it.toSeq.map(sample => {
          tileMatrixDatasetSampleToTileMatrix(sample)
        })
      }
    
      TileMatrixSet(id, tileMatrices)
    }
        
    /**
     * Dataset describing the TileMatrixSet from the WMTSCapabilities.xml
     * Identifier -> (TileWidth, TileHeight, MatrixWidth, MatrixHeight, tile_width_deg, tile_height_deg)
     */
    private lazy val dataset: Dataset = {
      //TODO: use id to query WmtsCapabilities Dataset?
      val dsName = "gibs_epsg4326_250m_tile_matrix_set"
      //val dsName = getProperty("tile_matrix_dataset", defaultTimeMatrix)
      val reader = DatasetAccessor.fromName(dsName)
      var ds: Dataset = null
      try { reader.getDataset().force }
      finally { reader.close() }
    }
  }
  
  //-------------------------------------------------------------------------//
  
  case class TileMatrix(level: Int, tileWidth: Int, tileHeight: Int, 
                        matrixWidth: Int, matrixHeight: Int) {
    
    /**
     * Return the spatial extent of the given tile as a GeoBoundingBox.
     */
    def getTileBoundingBox(tile: Tile): GeoBoundingBox = {
      val tileWidthDegrees:  Double = 360.0 / matrixWidth
      val tileHeightDegrees: Double = 180.0 / matrixHeight
      val minLon = tileWidthDegrees * tile.column - 180
      val maxLon = minLon + tileWidthDegrees
      val maxLat = 90.0 - tileHeightDegrees * tile.row
      val minLat = maxLat - tileHeightDegrees
      
      GeoBoundingBox(minLon, maxLon, minLat, maxLat)
    }
    
    def getTile(lon: Double, lat: Double): Tile = {
      //TODO: overload with GeoLocation?
      //inclusive on W and N, exclusive on E and S
      val row = Math.floor((0.5 - lat/180.0) * matrixHeight).toInt
      val col = Math.floor((0.5 + lon/360.0) * matrixWidth).toInt
      
      //If we are at the east or south edge of the matrix, don't add another tile.
      //TODO: precision concerns?
      val r = if (lat == -90.0) row - 1 else row
      val c = if (lon == 180.0) col - 1 else col
      
      Tile(level, r, c)
    }
    
    def getTiles(box: GeoBoundingBox): Seq[Tile] = {
      //get the tile coordinates for the NW and SE corners and fill in the gaps
      //TODO: don't include tiles on E and S if the box edge is on the tile boundary
      val w = matrixWidth
      val h = matrixHeight
      box match {
        case GeoBoundingBox(nw: GeoLocation, se: GeoLocation) => {
          val (row1, col1) = getTile(nw.longitude, nw.latitude) match { 
            case Tile(_, r, c) => (r,c) 
          }
          val (row2, col2) = getTile(se.longitude, se.latitude) match { 
            case Tile(_, r, c) => (r,c) 
          }
          for (row <- row1 to row2; col <- col1 to col2) yield Tile(level, row, col)
        }
      }
    }
    
  }
  
  //-------------------------------------------------------------------------//
  
  case class Tile(level: Int, row: Int, column: Int) {    
    override def toString = s"${level}/${row}/${column}"
  }
  
  //-------------------------------------------------------------------------//
  
  case class Layer(title: String, tileMatrixSet: TileMatrixSet)
}
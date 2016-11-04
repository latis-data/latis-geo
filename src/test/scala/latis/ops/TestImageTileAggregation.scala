package latis.ops

import org.junit.{Test,Ignore}
import org.junit.Assert.assertEquals
import latis.ops.agg.ImageTileAggregation
import latis.reader.DatasetAccessor
import latis.writer.Writer
import latis.dm._
import latis.metadata.Metadata
import latis.reader.ImageReader
import latis.writer.AsciiWriter

class TestImageTileAggregation {
  
  @Test @Ignore //visual test
  def horizontal {
    val ds1 = DatasetAccessor.fromName("tile1").getDataset
    val ds2 = DatasetAccessor.fromName("tile2").getDataset
    val joined = new ImageTileAggregation()(ds1, ds2)
    
    Writer("~/tmp/tiles.tif").write(joined)
  }
  
  @Test @Ignore //visual test
  def vertical {
    val ds1 = DatasetAccessor.fromName("tile1").getDataset
    val ds2 = DatasetAccessor.fromName("tile3").getDataset
    val joined = new ImageTileAggregation()(ds1, ds2)
    
    Writer("~/tmp/tiles.tif").write(joined)
  }
  
  @Test
  def simple_h {
    val joined = new ImageTileAggregation()(tile1, tile2)
    val data = joined.toDoubleMap
    
    val explon = List(0,1)
    val explat = List(0,1)
    val expa = List(1,2,5,6,3,4,7,8)
    
    assert(explon.equals(data("row").toSeq.distinct))
    assert(explat.equals(data("col").toSeq.distinct))
    assert(expa.equals(data("a").toSeq))
  }
  
  @Test
  def simple_v {
    val joined = new ImageTileAggregation()(tile1, tile3)
    val data = joined.toDoubleMap
    
    val explon = List(0,1)
    val explat = List(0,1)
    val expa = List(1,2,5,6,9,10,13,14)
    
    assert(explon.equals(data("row").toSeq.distinct))
    assert(explat.equals(data("col").toSeq.distinct))
    assert(expa.equals(data("a").toSeq))
  }
  
  @Test @Ignore // (expected=classOf[IllegalArgumentException])
  def mismatched {
    val joined = new ImageTileAggregation()(tile2, tile3)
  }
  
  @Test
  def seq_unordered {
    val joined = new ImageTileAggregation()(Seq(tile3, tile2, tile4, tile1))
    val data = joined.toDoubleMap
    
    val explon = List(0,1)
    val explat = List(0,1)
    val expa = List.range(1,17)
    
    assert(explon.equals(data("row").toSeq.distinct))
    assert(explat.equals(data("col").toSeq.distinct))
    assert(expa.equals(data("a").toSeq))
  }
  
  @Test
  def seq_ordered {
    val joined = new ImageTileAggregation()(Seq(tile1, tile2, tile3, tile4))
    //latis.writer.Writer.fromSuffix("asc").write(joined)
    val data = joined.toDoubleMap
    
    val explon = List(0,1)
    val explat = List(0,1)
    val expa = List.range(1,17)
    
    assert(explon.equals(data("row").toSeq.distinct))
    assert(explat.equals(data("col").toSeq.distinct))
    assert(expa.equals(data("a").toSeq))
  }

  //---- Tile tests with 2x2 tiles in row,column space. --------------------------------------------------
  /* tile1   tile2
   *  1  2    3  4    3
   *  5  6    7  8    2
   *                    lat
   *  9 10   11 12    1
   * 13 14   15 16    0
   * tile3   tile4
   * 
   *  0  1    2  3
   *   longitude
   * 
   * For geo-referencing, add corners (minLon, minLat) to metadata.
   * Assume 1 degree pixels with (0,0) in the lower left.
   */
  
  //same domain set for each tile
  val domainTuples = {
    val f = (p: (Int, Int)) => Tuple(Integer(Metadata("row"), p._1), Integer(Metadata("col"), p._2))
    Seq((0,0), (0, 1), (1,0), (1,1)).map(f) //row major order
  }

  lazy val tile1 = {
    val rangeValues = Seq(1,2,5,6).map(Integer(Metadata("a"), _))
    val fmd = Metadata("ncol" -> "2", "nrow" -> "2", "minLon" -> "0", "minLat" -> "2")
    Dataset(Function(domainTuples.zip(rangeValues).map(p => Sample(p._1, p._2)), fmd))
  }

  lazy val tile2 = {
    val rangeValues = Seq(3,4,7,8).map(Integer(Metadata("a"), _))
    val fmd = Metadata("ncol" -> "2", "nrow" -> "2", "minLon" -> "2", "minLat" -> "2")
    Dataset(Function(domainTuples.zip(rangeValues).map(p => Sample(p._1, p._2)), fmd))
  }

  lazy val tile3 = {
    val rangeValues = Seq(9,10,13,14).map(Integer(Metadata("a"), _))
    val fmd = Metadata("ncol" -> "2", "nrow" -> "2", "minLon" -> "0", "minLat" -> "0")
    Dataset(Function(domainTuples.zip(rangeValues).map(p => Sample(p._1, p._2)), fmd))
  }

  lazy val tile4 = {
    val rangeValues = Seq(11,12,15,16).map(Integer(Metadata("a"), _))
    val fmd = Metadata("ncol" -> "2", "nrow" -> "2", "minLon" -> "2", "minLat" -> "0")
    Dataset(Function(domainTuples.zip(rangeValues).map(p => Sample(p._1, p._2)), fmd))
  }
  

    
  //-----------------------------------------------------------------------------
    
  @Test @Ignore
  def join_wmts_tiles = {
    //http://kestrel:8080/latis-noms/latis/wmts_tiles.txt?time=2014-09-15&level=1
    //0, -180.0, -60.0, 0.0, 90.0, 2014-09-15/EPSG4326_250m/1/0/0.jpg
    //0, -60.0, 60.0, 0.0, 90.0, 2014-09-15/EPSG4326_250m/1/0/1.jpg
    //0, 60.0, 180.0, 0.0, 90.0, 2014-09-15/EPSG4326_250m/1/0/2.jpg
    //0, -180.0, -60.0, -90.0, 0.0, 2014-09-15/EPSG4326_250m/1/1/0.jpg
    //0, -60.0, 60.0, -90.0, 0.0, 2014-09-15/EPSG4326_250m/1/1/1.jpg
    //0, 60.0, 180.0, -90.0, 0.0, 2014-09-15/EPSG4326_250m/1/1/2.jpg
    val baseUrl = "http://map1.vis.earthdata.nasa.gov/wmts-geo/MODIS_Terra_CorrectedReflectance_TrueColor/default/"

    val ops1 = List(RowColToLonLat(-180.0, -60.0, 0.0, 90.0))
    val ds1 = ImageReader(baseUrl+"2014-09-15/EPSG4326_250m/1/0/0.jpg").getDataset(ops1)
    
    val ops2 = List(RowColToLonLat(-60.0, 60.0, 0.0, 90.0))
    val ds2 = ImageReader(baseUrl+"2014-09-15/EPSG4326_250m/1/0/1.jpg").getDataset(ops2)
    
    val ops3 = List(RowColToLonLat(-180.0, -60.0, -90.0, 0.0))
    val ds3 = ImageReader(baseUrl+"2014-09-15/EPSG4326_250m/1/1/0.jpg").getDataset(ops3)
    
    val ops4 = List(RowColToLonLat(-60.0, 60.0, -90.0, 0.0))
    val ds4 = ImageReader(baseUrl+"2014-09-15/EPSG4326_250m/1/1/1.jpg").getDataset(ops4)
    
    val ds = (new ImageTileAggregation())(List(ds1,ds2,ds3,ds4))
    
    //println(ds) //((longitude, latitude) -> (band0, band1, band2))
    //println(ds.getLength)  //1048576  (512 + 512)^2
    Writer("/tmp/modis_image2.tif").write(ds)
  }
}
package latis.ops

import org.junit.Ignore
import org.junit.Test
import latis.ops.agg.ImageTileAggregation
import latis.reader.DatasetAccessor
import latis.writer.Writer
import latis.dm._
import latis.metadata.Metadata
import latis.reader.ImageReader

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
    
    val explon = List(0,1,2,3)
    val explat = List(0,1)
    val expa = List(0,1,2,3,4,5,6,7)
    
    assert(explon.equals(data("longitude").toSeq.distinct))
    assert(explat.equals(data("latitude").toSeq.distinct))
    assert(expa.equals(data("a").toSeq))
  }
  
  @Test
  def simple_v {
    val joined = new ImageTileAggregation()(tile1, tile3)
    val data = joined.toDoubleMap
    
    val explon = List(0,1)
    val explat = List(0,1,2,3)
    val expa = List(0,1,4,5,8,9,12,13)
    
    assert(explon.equals(data("longitude").toSeq.distinct))
    assert(explat.equals(data("latitude").toSeq.distinct))
    assert(expa.equals(data("a").toSeq))
  }
  
  @Test(expected=classOf[IllegalArgumentException])
  def mismatched {
    val joined = new ImageTileAggregation()(tile2, tile3)
    val data = joined.toDoubleMap
  }
  
  @Test
  def seq_ordered {
    val joined = new ImageTileAggregation()(Seq(tile1, tile2, tile3, tile4))
    val data = joined.toDoubleMap
    
    val explon = List(0,1,2,3)
    val explat = List(0,1,2,3)
    val expa = List.range(0,16)
    
    assert(explon.equals(data("longitude").toSeq.distinct))
    assert(explat.equals(data("latitude").toSeq.distinct))
    assert(expa.equals(data("a").toSeq))
  }
  
  @Test
  def seq_unordered {
    val joined = new ImageTileAggregation()(Seq(tile3, tile2, tile4, tile1))
    val data = joined.toDoubleMap
    
    val explon = List(0,1,2,3)
    val explat = List(0,1,2,3)
    val expa = List.range(0,16)
    
    assert(explon.equals(data("longitude").toSeq.distinct))
    assert(explat.equals(data("latitude").toSeq.distinct))
    assert(expa.equals(data("a").toSeq))
  }
  
  lazy val tile1 = {
    val f = (p: (Int, Int)) => Tuple(Real(Metadata("longitude"), p._1), Real(Metadata("latitude"), p._2))
    val dom = Seq((0,0), (1, 0), (0,1), (1,1)).map(f)
    val ran = Seq(0,1,4,5).map(Real(Metadata("a"), _))
    
    Dataset(Function(dom.zip(ran).map(p => Sample(p._1, p._2))))
  }
  
  lazy val tile2 = {
    val f = (p: (Int, Int)) => Tuple(Real(Metadata("longitude"), p._1), Real(Metadata("latitude"), p._2))
    val dom = Seq((2,0), (3, 0), (2,1), (3,1)).map(f)
    val ran = Seq(2,3,6,7).map(Real(Metadata("a"), _))
    
    Dataset(Function(dom.zip(ran).map(p => Sample(p._1, p._2))))
  }
  
  lazy val tile3 = {
    val f = (p: (Int, Int)) => Tuple(Real(Metadata("longitude"), p._1), Real(Metadata("latitude"), p._2))
    val dom = Seq((0,2), (1, 2), (0,3), (1,3)).map(f)
    val ran = Seq(8,9,12,13).map(Real(Metadata("a"), _))
    
    Dataset(Function(dom.zip(ran).map(p => Sample(p._1, p._2))))
  }
  
  lazy val tile4 = {
    val f = (p: (Int, Int)) => Tuple(Real(Metadata("longitude"), p._1), Real(Metadata("latitude"), p._2))
    val dom = Seq((2,2), (3, 2), (2,3), (3,3)).map(f)
    val ran = Seq(10,11,14,15).map(Real(Metadata("a"), _))
    
    Dataset(Function(dom.zip(ran).map(p => Sample(p._1, p._2))))
  }
  
    
    
  @Test @Ignore //TODO: left/right looks good but top/bottom is reversed
  def join_wmts_tiles = {
    //http://kestrel:8080/latis-noms/latis/wmts_tiles.txt?time=2014-09-15&level=1
    //0, -180.0, -60.0, 0.0, 90.0, 2014-09-15/EPSG4326_250m/1/0/0.jpg
    //0, -60.0, 60.0, 0.0, 90.0, 2014-09-15/EPSG4326_250m/1/0/1.jpg
    //0, 60.0, 180.0, 0.0, 90.0, 2014-09-15/EPSG4326_250m/1/0/2.jpg
    //0, -180.0, -60.0, -90.0, 0.0, 2014-09-15/EPSG4326_250m/1/1/0.jpg
    //0, -60.0, 60.0, -90.0, 0.0, 2014-09-15/EPSG4326_250m/1/1/1.jpg
    //0, 60.0, 180.0, -90.0, 0.0, 2014-09-15/EPSG4326_250m/1/1/2.jpg
    val ops1 = List(RowColToLonLat(-180.0, -60.0, 0.0, 90.0))
    val baseUrl = "http://map1.vis.earthdata.nasa.gov/wmts-geo/MODIS_Terra_CorrectedReflectance_TrueColor/default/"
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
    Writer("/data/noms/modis_image.tif").write(ds)
  }
}
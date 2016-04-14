package latis.ops

import org.junit.Ignore
import org.junit.Test
import latis.ops.agg.ImageTileAggregation
import latis.reader.DatasetAccessor
import latis.writer.Writer
import latis.dm._
import latis.metadata.Metadata

class TestImageTileAggregation {
  
  @Test @Ignore //visual test
  def horizontal {
    val ds1 = DatasetAccessor.fromName("tile1").getDataset
    val ds2 = DatasetAccessor.fromName("tile2").getDataset
    val joined = new ImageTileAggregation()(ds1, ds2)
    
    Writer("/home/jast1399/test/tiles.tif").write(joined)
  }
  
  @Test @Ignore //visual test
  def vertical {
    val ds1 = DatasetAccessor.fromName("tile1").getDataset
    val ds2 = DatasetAccessor.fromName("tile3").getDataset
    val joined = new ImageTileAggregation()(ds1, ds2)
    
    Writer("/home/jast1399/test/tiles.tif").write(joined)
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
  
}
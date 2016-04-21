package latis.ops

import org.junit.Assert.assertEquals
import org.junit.Test
import latis.dm.Dataset
import latis.dm.Function
import latis.dm.Real
import latis.dm.Sample
import latis.dm.Tuple
import latis.metadata.Metadata
import org.geotools.factory.Hints
import org.junit.Ignore

class TestTransform {
  
  @Test
  def test {
    val t = Transform()
    val f = Dataset(Function(Seq(Sample(Real(Metadata("foo"),0.0), Tuple(Seq(Real(Metadata("x"), 996208.9809235458),
                                Real(Metadata("y"), -4541094.921570469),
                                Real(Metadata("z"), 4351898.060411415))))), Metadata(Map("crs" -> "EPSG:4978"))))
    val ds = t(f)
    val data = ds.toDoubleMap
    assertEquals(43.301, data("latitude")(0), 0.0000001)
    assertEquals(-77.626667, data("longitude")(0), 0.00000000001)
    assertEquals(0, data("altitude")(0), 0.01)
  }
  
  @Test 
  def general4978_to_4979 {
    val t = GeneralTransform("EPSG:4979")
    val f = Dataset(Function(Seq(Sample(Real(Metadata("foo"),0.0), Tuple(Seq(Real(Metadata("x"), 996208.9809235458),
                                Real(Metadata("y"), -4541094.921570469),
                                Real(Metadata("z"), 4351898.060411415))))), Metadata(Map("crs" -> "EPSG:4978"))))
    val ds = t(f)
    val data = ds.toDoubleMap
    assertEquals(43.301, data("latitude")(0), 0.0000001)
    assertEquals(-77.626667, data("longitude")(0), 0.00000000001)
    assertEquals(0, data("altitude")(0), 0.01)
  }
  
  @Test
  def general4979_to_4978 {
    val t = GeneralTransform("EPSG:4978")
    val f = Dataset(Function(Seq(Sample(Real(Metadata("foo"),0.0), Tuple(Seq(Real(Metadata("longitude"), -77.626667),
                                Real(Metadata("latitude"), 43.301),
                                Real(Metadata("altitude"), 0))))), Metadata(Map("crs" -> "EPSG:4979"))))
    val ds = t(f)
    val data = ds.toDoubleMap
    assertEquals(996208.9809235458, data("x")(0), 0.0001)
    assertEquals(-4541094.921570469, data("y")(0), 0.00004)
    assertEquals(4351898.060411415, data("z")(0), 0.0002)
  }
  
  @Test @Ignore //doesn't fail because of MappingIterator, but can't find the transformation
  def try2D_to_3D {
    val t = GeneralTransform("EPSG:4979")
    val f = Dataset(Function(Seq(Sample(Real(Metadata("foo"),0.0), Tuple(Seq(Real(Metadata("longitude"), -77.626667),
                                Real(Metadata("latitude"), 43.301))))), Metadata(Map("crs" -> "EPSG:4326"))))
    val ds = t(f)
    val data = ds.toDoubleMap
    assertEquals(43.301, data("latitude")(0), 0.0000001)
    assertEquals(-77.626667, data("longitude")(0), 0.00000000001)
    assertEquals(0, data("altitude")(0), 0.01)
  }
  
  @Test @Ignore //doesn't fail because of MappingIterator, but can't find the transformation
  def try3D_to_2D {
    val t = GeneralTransform("EPSG:4326")
    val f = Dataset(Function(Seq(Sample(Real(Metadata("foo"),0.0), Tuple(Seq(Real(Metadata("longitude"), -77.626667),
                                Real(Metadata("latitude"), 43.301),
                                Real(Metadata("altitude"), 0))))), Metadata(Map("crs" -> "EPSG:4979"))))
    val ds = t(f)
    val data = ds.toDoubleMap
    assertEquals(43.301, data("latitude")(0), 0.0000001)
    assertEquals(-77.626667, data("longitude")(0), 0.00000000001)
  }
  
}

//       x/lon              y/lat               z/alt
//4978:  996208.9809235458  -4541094.921570469  4351898.060411415
//4979:  -77.626667         43.301              0
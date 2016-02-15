package latis.ops

import org.junit.Assert.assertEquals
import org.junit.Test

import latis.dm.Dataset
import latis.dm.Function
import latis.dm.Real
import latis.dm.Sample
import latis.dm.Tuple
import latis.metadata.Metadata

class TestTransform {
  
  @Test
  def test {
    val t = Transform()
    val f = Dataset(Function(Seq(Sample(Tuple(Seq(Real(Metadata("x"), 996208.9809235458),
                                Real(Metadata("y"), -4541094.921570469),
                                Real(Metadata("z"), 4351898.060411415))), Real(Metadata("foo"),0.0)))))
    val ds = t(f)
    val data = ds.toDoubleMap
    assertEquals(43.301, data("latitude")(0), 0.0000001)
    assertEquals(-77.626667, data("longitude")(0), 0.00000000001)
    assertEquals(0, data("altitude")(0), 0.01)
  }
  
}
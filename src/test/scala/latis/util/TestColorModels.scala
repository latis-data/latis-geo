package latis.util

import org.junit.Test
import latis.dm._
import latis.metadata.Metadata
import latis.writer.Writer

class TestColorModels {
  
  @Test //visual test
  def temp {
    val domf = (p: (Int, Int)) => Tuple(Real(Metadata("longitude"), p._1), Real(Metadata("latitude"), p._2))
    val dom = Seq((0, 0), (0, 1), (1,0), (1,1)).map(domf(_))
    val ranmd = Metadata(Map("name" -> "temperature", "units" -> "Kelvin"))
    val ran = Seq(0.0, 273.0, 293.0, 373.0).map(v => Real(ranmd, v))
    val sam = dom.zip(ran).map(p => Sample(p._1, p._2))
    val f = Function(sam, Metadata(Map("color_model" -> "test")))
    
    Writer("/home/jast1399/test/color.tif").write(Dataset(f))
  }
  
}
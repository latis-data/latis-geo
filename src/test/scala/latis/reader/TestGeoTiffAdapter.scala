package latis.reader

import org.junit.Test

import latis.reader.tsml.TsmlReader
import latis.writer.AsciiWriter

class TestGeoTiffAdapter {
  
  //@Test
  def testread {
    val ds = TsmlReader("tiff.tsml").getDataset
    //AsciiWriter.write(ds)
  }

}
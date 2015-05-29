package latis.reader

import org.junit.Test
import latis.reader.tsml.TsmlReader
import latis.writer.AsciiWriter
import java.io.File
import latis.writer.GeoTiffWriter

class TestHdf {
  
  @Test
  def test {
    val ds = TsmlReader("hdf.tsml").getDataset
    AsciiWriter.write(ds)
  }
  
  @Test
  def test_write {
    val file = new File("src/test/resources/tif/testhdf.tif")
    try{
      val ds = TsmlReader("hdf.tsml").getDataset
      new GeoTiffWriter().writeFile(ds, file)
    }
    catch {
      case e: Exception => {
        file.delete
        throw e
      }
    }
    
  }

}
package latis.writer

import java.io.File

import org.junit.Test

import latis.reader.tsml.TsmlReader

class TestGeoTiffWriter {
  
  //@Test
  def test {
    val file = new File("src/test/resources/tif/test.tif")
    try{
      val ds = TsmlReader("tiff.tsml").getDataset
      val w = new GeoTiffWriter()
      w.setFile(file)
      w.write(ds)
    }
    catch {
      case e: Exception => {
        file.delete
        throw e
      }
    }
    
  }
  
}
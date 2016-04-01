package latis.util

import java.io.File
import org.junit.{Test,Before,Ignore}
import org.junit.Assert._
import org.geotools.styling.StyleFactory
import org.geotools.factory.CommonFactoryFinder
import org.opengis.filter.expression.Expression
import org.opengis.filter.FilterFactory
import latis.reader.DatasetAccessor
import latis.dm.Dataset
import latis.dm.Function


class TestWindMarkPointStyle {
  var sf: StyleFactory = null
  var ff: FilterFactory = null
  var cmps = null
  var style = null
  
  @Before
  def init {
    sf = CommonFactoryFinder.getStyleFactory
    ff = CommonFactoryFinder.getFilterFactory
  }
  
  /*
   * basic test to make sure style created has set size 10
   * this test will fail because we now try to look up a wind
   * scalar in latis.properties file. we need to bring mockito
   * to mock the request to that property
   */
  //@Test
  def circleMarkHasSizeTen {
    //val ds = DatasetAccessor.fromName("gfsanl_wind_files").getDataset()
    //val f = ds match {
    //  case Dataset(f: Function) => f
    //}
    
    val angle = 45.0
    val magnitued = 0
    val style = WindMarkPointStyle.getCustomWindSymbolizer(sf,angle,magnitued)
    val size = ff.literal(10)
    val rotation = ff.literal(45.0)
    assertEquals(size, style.getGraphic.getSize)
    assertEquals(rotation, style.getGraphic.getRotation)
  }
}
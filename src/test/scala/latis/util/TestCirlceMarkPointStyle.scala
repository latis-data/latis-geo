package latis.util

import java.io.File

import org.junit.{Test,Before,Ignore}
import org.junit.Assert._

import org.geotools.styling.StyleFactory
import org.geotools.factory.CommonFactoryFinder

import org.opengis.filter.expression.Expression
import org.opengis.filter.FilterFactory

class TestCircleMarkPointStyle {
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
   */
  @Test
  def circleMarkHasSizeTen {
    val style = CircleMarkPointStyle.getCustomPointCircleSymbolizer(sf)
    val size = ff.literal(10)
    assertEquals(size, style.getGraphic.getSize)
  }
}
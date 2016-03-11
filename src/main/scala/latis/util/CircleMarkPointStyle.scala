package latis.util

import org.geotools.styling.StyleFactoryFinder
import org.geotools.filter.FilterFactoryFinder
import java.awt.Color
import org.geotools.styling.Rule
import org.geotools.styling.Style
import org.geotools.styling.Symbolizer
import org.opengis.style.GraphicalSymbol
import org.geotools.styling.AnchorPoint
import org.geotools.styling.Displacement
import org.opengis.filter.expression.Expression
import scala.collection.JavaConverters._
import org.geotools.styling.PointSymbolizer
import org.geotools.styling.StyleFactory

object CircleMarkPointStyle {
  
  def getCustomPointCircleSymbolizer(sf: StyleFactory): PointSymbolizer = {
    val filterFactory = FilterFactoryFinder.createFilterFactory
    
    // controls            color                                      line width
    val stroke = sf.stroke(filterFactory.literal(Color.ORANGE), null, filterFactory.literal(.5), null, null, null, null)
    // controls              color opacity (0 to 1)
    val fill = sf.fill(null, null, null) // ie. no fill so we get open circles
    
    val sl = scala.collection.mutable.ListBuffer[GraphicalSymbol](sf.mark(filterFactory.literal("circle"), fill, stroke))
    val symbols: java.util.List[GraphicalSymbol] = sl.asJava
    
    val opacity: Expression = null
    val size: Expression = filterFactory.literal(10) // controls the size of the circle
    val rotation: Expression = null
    val anchor: AnchorPoint = null
    val displacement: Displacement = null
    
    val circle = sf.graphic(symbols, opacity, size, rotation, anchor, displacement)
    val pointSymbolizer = sf.pointSymbolizer("point", filterFactory.property("point"), null, null, circle)
    
    pointSymbolizer
  }

}
package latis.util

import java.awt.Color
import org.opengis.style.GraphicalSymbol
import org.opengis.filter.expression.Expression
import org.opengis.filter.expression.Expression
import org.geotools.styling.Rule
import org.geotools.styling.Style
import org.geotools.styling.Symbolizer
import org.geotools.styling.AnchorPoint
import org.geotools.styling.Displacement
import org.geotools.styling.PointSymbolizer
import org.geotools.styling.StyleFactory
import org.geotools.styling.StyleFactoryFinder
import scala.collection.JavaConverters._
import org.geotools.factory.CommonFactoryFinder
import org.geotools.renderer.style.MarkFactory
import org.geotools.renderer.style.TTFMarkFactory
import org.geotools.renderer.style.WellKnownMarkFactory
import org.geotools.renderer.style.ShapeMarkFactory
import latis.dm.Function


object WindMarkPointStyle {
  
  def getCustomWindSymbolizer(sf: StyleFactory,function: Function): PointSymbolizer = {
    val filterFactory = CommonFactoryFinder.getFilterFactory
    
    // controls            color                                      line width
    val stroke = sf.stroke(filterFactory.literal(Color.ORANGE), null, filterFactory.literal(.5), null, null, null, null)
    // controls              color opacity (0 to 1)
    val fill = sf.fill(null, null, null) // no fill so we get open arrows
    
    val mark = sf.mark(filterFactory.literal("extshape://arrow"), fill, stroke)
    
    // needed to do this since sf.graphic() below requries a java list
    val sl = scala.collection.mutable.ListBuffer[GraphicalSymbol](mark)
    val symbols: java.util.List[GraphicalSymbol] = sl.asJava
    
    val opacity: Expression = null
    val size: Expression = filterFactory.literal(10) // controls the size of the arrow
    val rotation: Expression = filterFactory.literal(45) // controls the rotation from north of the arrow
    val anchor: AnchorPoint = null
    val displacement: Displacement = null
    
    val arrow = sf.graphic(symbols, opacity, size, rotation, anchor, displacement)
    val pointSymbolizer = sf.pointSymbolizer("point", filterFactory.property("point"), null, null, arrow)
    
    pointSymbolizer
  }
}
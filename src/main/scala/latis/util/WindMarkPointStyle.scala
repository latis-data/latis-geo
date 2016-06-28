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
import latis.dm.Tuple
import latis.dm.Sample
import latis.dm.Real
import latis.dm.Number
import org.geotools.styling.Graphic
import org.geotools.styling.StyleBuilder


object WindMarkPointStyle {
  
  def getCustomWindSymbolizer(sf: StyleFactory, angle: Double, speed: Double): PointSymbolizer = {
    val filterFactory = CommonFactoryFinder.getFilterFactory
    val sb = new StyleBuilder
    // controls            color                                      line width
    val stroke = sf.stroke(filterFactory.literal(Color.CYAN), null, filterFactory.literal(.5), null, null, null, null)
    // controls              color opacity (0 to 1)
    val fill = sf.fill(null, null, null) // no fill so we get open arrows
    
    val mark = LatisProperties.get("noms.wind.style") match {
      case Some(s) if (s == "barb")   => sf.mark(filterFactory.literal("windbarbs://default("+ speed +")[kts]"), fill, stroke) //Wind Barb
      case Some(s) if (s == "vector") => sf.mark(filterFactory.literal("extshape://arrow"), fill, stroke)                      //Wind Vector  
      case None => throw new Error("The noms.wind.style property is undefined.")  
      case _    => throw new Error("The noms.wind.style property is not properly defined.")
    }
    
    // needed to do this since sf.graphic() below requries a java list
    val sl = scala.collection.mutable.ListBuffer[GraphicalSymbol](mark)
    val symbols: java.util.List[GraphicalSymbol] = sl.asJava
    
    val opacity: Expression = null
    
    val windbarbsize = LatisProperties.get("noms.wind.scale") match {
      case Some(s) => s.toInt
      case None => throw new Error("The noms.wind.scale property is undefined.") 
    }
    val size: Expression = filterFactory.literal(windbarbsize) // controls the size of the arrow
    val rotation: Expression = filterFactory.literal(angle) // controls the rotation from north of the arrow
    val anchor: AnchorPoint = null //sb.createAnchorPoint(filterFactory.literal(36.5), filterFactory.literal(122.5))
    val displacement: Displacement = null
    
    val arrow = sf.graphic(symbols, opacity, size, rotation, anchor, displacement)
    val pointSymbolizer = sf.pointSymbolizer("point", filterFactory.property("point"), null, null, arrow)
    
    pointSymbolizer
  }
  
}
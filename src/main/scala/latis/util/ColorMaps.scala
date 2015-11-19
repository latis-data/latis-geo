package latis.util

import java.awt.image.IndexColorModel
import java.awt.image.ColorModel

object ColorModels {
  
  def apply(name: String): ColorModel = name match {
    case "clouds" => percent10
    case "redscale" => redscale
    case _ => throw new UnsupportedOperationException(s"$name not recognized as a color model name.")
  }
  
  /**
   * Colors ranging from cool to warm at ten percent intervals
   * (with gray for 0 and white for everything above 100). 
   */
  lazy val percent10 = {
    val colors = Array(Array(0x66, 0x00, 0x66), Array(0x29, 0x6b, 0xc1), Array(0x00, 0xc0, 0xcb),
                       Array(0x00, 0xef, 0x32), Array(0x65, 0xff, 0x00), Array(0xff, 0xff, 0x00),
                       Array(0xff, 0xd1, 0x01), Array(0xff, 0x8e, 0x01), Array(0xff, 0x23, 0x00),
                       Array(0xb7, 0x01, 0x00))
    val map = Array.fill(256)(Array(0xff, 0xff, 0xff))
    for(i <- Range(1, 100)) map(i) = colors(i/10)
    map(0) = Array(0x96, 0x96, 0x96)
    new IndexColorModel(8, 256, map.flatMap(_.map(_.toByte)), 0, false)
  }
  
  lazy val redscale = {
    val map = Array.tabulate(256)(i => Array(i, 0, 0))
    new IndexColorModel(8, 256, map.flatMap(_.map(_.toByte)), 0, false)
  }
}
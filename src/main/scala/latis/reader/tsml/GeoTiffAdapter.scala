package latis.reader.tsml

import org.geotools.gce.geotiff.GeoTiffReader

import com.typesafe.scalalogging.LazyLogging

import latis.reader.tsml.ml.Tsml

class GeoTiffAdapter(tsml: Tsml) extends ImageAdapter(tsml) with LazyLogging{
  
  override lazy val reader: GeoTiffReader = new GeoTiffReader(getUrl)
  
}
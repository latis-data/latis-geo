package latis.reader.tsml

import java.awt.geom.Point2D
import java.awt.image.Raster
import latis.reader.tsml.ml.Tsml
import org.geotools.gce.geotiff.GeoTiffReader
import org.geotools.coverage.grid.GridCoverage2D
import org.opengis.referencing.operation.MathTransform2D
import latis.data.Data
import latis.metadata.Metadata
import latis.reader.tsml.ml.VariableMl
import latis.reader.tsml.ml.FunctionMl
import org.geotools.referencing.CRS
import com.typesafe.scalalogging.LazyLogging

class GeoTiffAdapter(tsml: Tsml) extends ImageAdapter(tsml) with LazyLogging{
  
  override lazy val reader: GeoTiffReader = new GeoTiffReader(getUrl)
  
}
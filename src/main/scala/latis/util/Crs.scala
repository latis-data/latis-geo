package latis.util

import org.geotools.factory.Hints
import org.geotools.referencing.CRS
import org.geotools.referencing.crs.DefaultGeocentricCRS
import org.opengis.referencing.crs.CoordinateReferenceSystem

import latis.dm.Function

object Crs {
  
  Hints.putSystemDefault(Hints.FORCE_LONGITUDE_FIRST_AXIS_ORDER, true)
  
  def decode(code: String) = 
    CRS.decode(code)
  
  def findMathTransform(source: CoordinateReferenceSystem, target: CoordinateReferenceSystem) = 
    CRS.findMathTransform(source, target)
    
  def lookupEpsgCode(crs: CoordinateReferenceSystem, fullScan: Boolean) = 
    CRS.lookupEpsgCode(crs, fullScan)
  
  def equalsIgnoreMetadata(crs1: CoordinateReferenceSystem, crs2: CoordinateReferenceSystem) = 
    CRS.equalsIgnoreMetadata(crs1, crs2)
  
  /**
   * Get the CoordinateReferenceSystem defined by epsg code
   * in this Function's Metadata. If no "epsg" metadata is defined,
   * default to WGS84 (epsg:4326).
   */
  def getCrs(function: Function): CoordinateReferenceSystem = {
    function.getMetadata("crs") match {
      case Some("EPSG:404000") => CRS.decode("EPSG:4326") //404000 is an unusable default when reading images
      case Some("EPSG:4978") => DefaultGeocentricCRS.CARTESIAN //override to allow transformations
      case Some(s) => CRS.decode(s)
      case None => CRS.decode("EPSG:4326")
    }
  }
  
}
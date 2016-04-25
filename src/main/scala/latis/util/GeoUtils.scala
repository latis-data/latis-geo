package latis.util

object GeoUtils {
  
  case class GeoBoundingBox(nw: GeoLocation, se: GeoLocation) 

  
  object GeoBoundingBox {
    
    def apply(minLon: Double, maxLon: Double, minLat: Double, maxLat: Double): GeoBoundingBox = {
      //Note, this will adjust longitude to be in [-180,180].
      new GeoBoundingBox(GeoLocation(minLon, maxLat), GeoLocation(maxLon, minLat))
    }
  }
  
  //TODO: consider org.opengis.metadata.extent.GeographicBoundingBox or Rectangle or Envelope
}
package latis.util

/*
 * TODO: extend Tuple, trait? new AbstractTuple with GeoLocation
 * how to make it dynamically from tsml? 
 *   companion object constructor?
 */

class GeoLocation private (val longitude: Double, val latitude: Double) {
  //TODO: support any longitude for wrapping, default to the dateline (-180 = 180)
  //TODO: impl order?, assume row-major (lon varies slowest)
  //TODO: impl equals, deal with -180 = 180
}

object GeoLocation {

  def apply(longitude: Double, latitude: Double): GeoLocation = {
    //enforce longitude = [-180,180] by adjusting 
    //Note that we allow 180 to be included in this context so we can avoid a bit of the dateline problem 
    //TODO: reconcile with latis.ops.AdjustLongitude
    val dMod360 = (longitude % 360)
    val lon = if (dMod360 > 180) dMod360 - 360
              else if (dMod360 < -180) dMod360 + 360
              else dMod360
            
    //enforce latitude = [-90,90]
    val lat = if (latitude >= -90.0 && latitude <= 90.0) latitude
    else throw new Error("Latitude not between -90 and 90: " + latitude)
    
    new GeoLocation(lon, lat)
  }
  
  def unapply(location: GeoLocation) = Some((location.longitude, location.latitude))
}
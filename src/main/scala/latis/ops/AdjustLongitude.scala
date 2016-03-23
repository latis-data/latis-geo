package latis.ops

import latis.dm.Number
import latis.dm.Scalar
import latis.data.value.DoubleValue
import latis.dm.Real
import latis.dm.Sample

/**
 * Adjust longitudes to be from -180 to 180 degrees east.
 * Inclusive at -180 but exclusive at 180: [-180, 180)
 * Assumes they are already degrees east.
 */
class AdjustLongitude extends Operation {
  //TODO: should this be exclusive at 180: [-180,180) ?
  
  /**
   * Apply to both domain and range.
   */
  override def applyToSample(sample: Sample): Option[Sample] = {
    for (domain <- applyToVariable(sample.domain);
         range <- applyToVariable(sample.range)
    ) yield Sample(domain, range)
  }
  
  /**
   * If the given scalar is a longitude, make sure its value is in the range [-180,180].
   * The scalar must be named "longitude" or have a "longitude" alias.
   * Assumes units are "degrees_east".
   */
  override def applyToScalar(scalar: Scalar): Some[Scalar] = {
    if (scalar.hasName("longitude")) scalar match {
      //TODO: optimize: don't make new scalar if we don't need to?
      //TODO: munge metadata?
      case Number(d) => {
        val dMod360 = (d % 360)
        val d2 = if (dMod360 >= 180) dMod360 - 360
            else if (dMod360 < -180) dMod360 + 360
            else dMod360
        Some(Real(scalar.getMetadata, DoubleValue(d2))) //TODO: use copy, preserve int
      }
    }
    else Some(scalar)  //no-op
  }
}

object AdjustLongitude {
  
  def apply(): AdjustLongitude = new AdjustLongitude()
}
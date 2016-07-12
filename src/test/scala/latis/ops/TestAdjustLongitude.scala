package latis.ops

import org.junit.Assert.assertEquals
import org.junit.Test

import latis.dm.Dataset
import latis.dm.Function
import latis.dm.Real
import latis.dm.Sample
import latis.dm.Tuple
import latis.metadata.Metadata

/**
 * Scalars with name or alias "longitude" should end up in the range [-180,180)
 */
class TestAdjustLongitude {
  
  @Test
  def zero = {
    val v = Real(Metadata("longitude"), 0.0)
    AdjustLongitude()(Dataset(v)) match {
      case Dataset(Real(d)) => assertEquals(0.0, d, 0.0)
    }
  }
  
  @Test
  def at_180 = {
    val v = Real(Metadata("longitude"), 180.0)
    AdjustLongitude()(Dataset(v)) match {
      case Dataset(Real(d)) => assertEquals(-180.0, d, 0.0)
    }
  }
  
  @Test
  def at_neg180 = {
    val v = Real(Metadata("longitude"), -180.0)
    AdjustLongitude()(Dataset(v)) match {
      case Dataset(Real(d)) => assertEquals(-180.0, d, 0.0)
    }
  }
  
  @Test
  def between_180_360 = {
    val v = Real(Metadata("longitude"), 200.0)
    AdjustLongitude()(Dataset(v)) match {
      case Dataset(Real(d)) => assertEquals(-160.0, d, 0.0)
    }
  }
  
  @Test
  def over_360 = {
    val v = Real(Metadata("longitude"), 400.0)
    AdjustLongitude()(Dataset(v)) match {
      case Dataset(Real(d)) => assertEquals(40.0, d, 0.0)
    }
  }
  
  @Test
  def between_neg180_0 = {
    val v = Real(Metadata("longitude"), -100.0)
    AdjustLongitude()(Dataset(v)) match {
      case Dataset(Real(d)) => assertEquals(-100.0, d, 0.0)
    }
  }
  
  @Test
  def between_neg360_neg180 = {
    val v = Real(Metadata("longitude"), -200.0)
    AdjustLongitude()(Dataset(v)) match {
      case Dataset(Real(d)) => assertEquals(160.0, d, 0.0)
    }
  }
  
  @Test
  def below_neg360 = {
    val v = Real(Metadata("longitude"), -400.0)
    AdjustLongitude()(Dataset(v)) match {
      case Dataset(Real(d)) => assertEquals(-40.0, d, 0.0)
    }
  }
  
  @Test
  def not_longitude = {
    val v = Real(Metadata("foo"), -400.0)
    AdjustLongitude()(Dataset(v)) match {
      case Dataset(Real(d)) => assertEquals(-400.0, d, 0.0)
    }
  }
  
  //integer
  
}
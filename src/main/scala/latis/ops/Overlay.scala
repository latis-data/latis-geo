package latis.ops

import latis.dm._
import latis.reader.DatasetAccessor

class Overlay(layer: Dataset) extends Operation {
  
  override def apply(ds: Dataset) = {
    (ds, layer) match {
      case (ds1: Dataset, ds2: Dataset) if (ds2.isEmpty) => ds1
      case (ds1: Dataset, ds2: Dataset) if (ds1.isEmpty) => ds2
      case (Dataset(f1: Function), Dataset(f2: Function)) => 
        Dataset(Tuple(f1, f2), ds.getMetadata)
      case (Dataset(Tuple(vs1)), Dataset(f2: Function)) => 
        Dataset(Tuple(vs1 :+ f2), ds.getMetadata)
      case (Dataset(f1: Function), Dataset(Tuple(vs2))) => 
        Dataset(Tuple(f1 +: vs2), ds.getMetadata)
      case (Dataset(Tuple(vs1)), Dataset(Tuple(vs2))) => 
        Dataset(Tuple(vs1 ++ vs2), ds.getMetadata)
    }
  }
  
  /**
   * Combine two Overlays into one
   */
  def apply(ol: Overlay): Overlay = Overlay(ol(layer))
  
}

object Overlay extends OperationFactory {
  
  def apply(ds: Dataset): Overlay = new Overlay(ds)
  
}
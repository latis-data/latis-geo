package latis.ops

import latis.dm._

class Overlay(layer: Dataset) extends Operation {
  
  override def apply(ds: Dataset) = {
    (ds, layer) match {
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
  
}
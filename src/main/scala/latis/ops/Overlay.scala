package latis.ops

import latis.dm.Dataset
import latis.dm.Function
import latis.dm.Tuple
import latis.ops.agg.Join

class Overlay extends Join {
  //TODO: generalize (LATIS-489)
  //  consider a simple join with a Tuple then flatten
  //TODO: update metadata
  
  override def apply(base: Dataset, layer: Dataset): Dataset = {
    (base, layer) match {
      case (ds1: Dataset, ds2: Dataset) if (ds2.isEmpty) => ds1
      case (ds1: Dataset, ds2: Dataset) if (ds1.isEmpty) => ds2
      case (Dataset(f1: Function), Dataset(f2: Function)) => 
        Dataset(Tuple(f1, f2), base.getMetadata)
      case (Dataset(Tuple(vs1)), Dataset(f2: Function)) => 
        Dataset(Tuple(vs1 :+ f2), base.getMetadata)
      case (Dataset(f1: Function), Dataset(Tuple(vs2))) => 
        Dataset(Tuple(f1 +: vs2), base.getMetadata)
      case (Dataset(Tuple(vs1)), Dataset(Tuple(vs2))) => 
        Dataset(Tuple(vs1 ++ vs2), base.getMetadata)
    }
  }
  
}

object Overlay {
  
  def apply(): Overlay = new Overlay()
  
  def apply(ds1: Dataset, ds2: Dataset): Dataset = Overlay()(ds1, ds2)
  
  def apply(dss: Seq[Dataset]): Dataset = Overlay()(dss)
  
}
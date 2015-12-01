package latis.ops

import latis.dm._
import latis.reader.DatasetAccessor

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

object Overlay extends OperationFactory {
  
  def apply(ds: Dataset): Overlay = new Overlay(ds)
  
  def apply(name: String): Overlay = {
    val ds = DatasetAccessor.fromName(name).getDataset
    Overlay(ds)
  }
  
  override def apply(args: Seq[String]) = Overlay(args.head)
  
}
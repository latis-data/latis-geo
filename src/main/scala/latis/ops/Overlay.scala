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
  
  /**
   * Combine two Overlays into one
   */
  def apply(ol: Overlay): Overlay = Overlay(ol(layer))
  
}

object Overlay extends OperationFactory {
  
  def apply(ds: Dataset): Overlay = new Overlay(ds)
  
  def apply(name: String): Overlay = {
    val reader = DatasetAccessor.fromName(name)
    try {
      val ds = reader.getDataset.force
      Overlay(ds)
    } finally {
      reader.close()
    }
    
    
    
  }
  
  override def apply(args: Seq[String]) = args.length match {
    case 1 => Overlay(args.head)
    case _ => args.map(Overlay(_)).reduceLeft(_(_))
  }
  
}
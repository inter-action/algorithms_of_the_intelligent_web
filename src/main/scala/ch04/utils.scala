package iweb.ch04.utils

import scala.collection.mutable

object TermFrequencyBuilder {
  def buildTermFrequencyVectors(x: Array[String], y: Array[String]): (Array[Double], Array[Double]) ={
    val allAttrs = mutable.LinkedHashMap(x.map( (_, 0x01) ): _*)
    
    // this would not work cause .toMap donest guarantee insert order
    // val allAttrs = mutable.HashMap.empty[String, Int] ++= x.map((_, 0x01)).toMap

    y.map { e=>
      if (allAttrs.contains(e)){
        // set flags to indicate that this term is present only in y[]
        allAttrs.put(e, 0x03)
      }else{
        // set flags to indicate that this term is present in x[] and y[]
        allAttrs.put(e, 0x02)
      }
    }

    val arrs = allAttrs.toArray

    val rs: Array[(Double, Double)] = for ( idx <- arrs.indices.toArray ) yield {
      val (key, value) = arrs(idx)
      val termFrequencyX = value & 0x01
      val termFrequencyY = value >> 1
      (termFrequencyX.toDouble, termFrequencyY.toDouble)
    }

    rs.unzip
  }

}

class ObjectToIndexMapping[T]{
  private var nextIndex = 0

  private val objectMapping: mutable.HashMap[T, Int] = mutable.HashMap.empty[T, Int]
  private val indexMapping: mutable.HashMap[Int, T] = mutable.HashMap.empty[Int, T]

  def getObject(idx: Int): Option[T] = indexMapping.get(idx)
  def getIndex(obj: T): Int = {
    val idx = objectMapping.get(obj)
    if (idx.isEmpty){
      objectMapping.put(obj, nextIndex)
      indexMapping.put(nextIndex, obj)
      nextIndex += 1
      nextIndex - 1
    }else{
      idx.get
    }
  }
}









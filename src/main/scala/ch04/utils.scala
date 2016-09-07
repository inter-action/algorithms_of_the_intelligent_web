package iweb.ch04.utils

import scala.collection.mutable

object TermFrequencyBuilder {
  def buildTermFrequencyVectors(x: Array[String], y: Array[String]): Array[Array[Double]] ={
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

    val rs = for ( idx <- arrs.indices ) yield {
      val (key, value) = arrs(idx)
      val termFrequencyX = value & 0x01
      val termFrequencyY = value >> 1
      (termFrequencyX.toDouble, termFrequencyY.toDouble)
    }

    val k = rs.unzip
    Array(k._1.toArray, k._2.toArray)
  }

}
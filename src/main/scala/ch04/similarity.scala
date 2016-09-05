package iweb.ch04.similarity

import scala.collection.immutable.HashSet


trait Distance{
  def getDistance(x: Array[Double], y: Array[Double]): Double
}

object EuclideanDistance extends Distance{
  override def getDistance(x: Array[Double], y: Array[Double]): Double = {
    val sum = x.zip(y).foldLeft(0.0){ (acc, e)=> acc + Math.pow(e._1 - e._2, 2) }
    Math.sqrt(sum)
  }
}



trait SimilarityMeasure {
  def similarity(x: Array[String], y: Array[String]): Double
}

object JaccardCoefficient extends SimilarityMeasure{
  override def similarity(x: Array[String], y: Array[String]): Double = {
    if (x.size == 0 || y.size == 0) return 0.0


    val union = HashSet(x ++ y)
    val intersect = HashSet(x).intersect(HashSet(y))

    intersect.size.toDouble / union.size
  }
}


object CosineSimilarity extends SimilarityMeasure{
  //todo:
  override def similarity(x: Array[String], y: Array[String]): Double = ???
}
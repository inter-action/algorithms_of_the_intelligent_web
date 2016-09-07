package iweb.ch04.similarity

import iweb.ch04.utils.TermFrequencyBuilder

import scala.collection.immutable.HashSet


trait Distance {
  def getDistance(x: Array[Double], y: Array[Double]): Double
}

object EuclideanDistance extends Distance {
  override def getDistance(x: Array[Double], y: Array[Double]): Double = {
    val sum = x.zip(y).foldLeft(0.0) { (acc, e) => acc + Math.pow(e._1 - e._2, 2) }
    Math.sqrt(sum)
  }
}


object CosineDistance extends Distance {
  override def getDistance(x: Array[Double], y: Array[Double]): Double = {
    val sim = CosineSimilarity.sim(x, y)

    assert(sim > 0.0, s"cant use this value to calculate distance. x[]=${x}, y[]=${y}, cosin.sim(x, y)=${sim} ")
    return 1.0 - sim
  }
}


trait SimilarityMeasure {
  def similarity(x: Array[String], y: Array[String]): Double
}

object JaccardCoefficient extends SimilarityMeasure {
  override def similarity(x: Array[String], y: Array[String]): Double = {
    if (x.size == 0 || y.size == 0) return 0.0


    val union = HashSet(x ++ y)
    val intersect = HashSet(x).intersect(HashSet(y))

    intersect.size.toDouble / union.size
  }
}


object CosineSimilarity extends SimilarityMeasure {
  override def similarity(x: Array[String], y: Array[String]): Double = {
    val (termFrequencyForX, termFrequencyForY) = TermFrequencyBuilder.buildTermFrequencyVectors(x, y)

    sim(termFrequencyForX, termFrequencyForY)

  }

  def sim(x: Array[Double], y: Array[Double]): Double = {
    val docProduct = x.indices.foldLeft(0.0) { (acc, idx) =>
      acc + x(idx) * y(idx)
    }

    docProduct / (getNorm(x) * getNorm(y))
  }

  def getNorm(v: Array[Double]) = Math.sqrt(v.foldLeft(0.0)((acc, v) => acc + v * v))
}
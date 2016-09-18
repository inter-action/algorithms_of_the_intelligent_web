package iweb.ch04.rock

import java.util

import iweb.ch04.models.{Cluster, WordDataPoint}
import iweb.ch04.similarity.SimilarityMeasure
import iweb.ch04.utils.ObjectToIndexMapping

/*
todo:
  what is links: @page 149

  这个links到底是什么用的, @nLinksBetweenPoints method
 */

/**
  *
  * @param points 特征点
  * @param similarityMatrix
  * @param threshold
  */
class LinkMatrix(points: Array[WordDataPoint], similarityMatrix: Array[Array[Double]], threshold: Double) {
  private val objToIndexMapping = new ObjectToIndexMapping[WordDataPoint]
  // link counts result matrix
  // link counts result matrix
  private val pointLinkMatrix = Array.fill(points.length, points.length)(0)
  private val pointNeighborMatrix = Array.fill(points.length, points.length)(0)

  def this(points: Array[WordDataPoint], pointSim: SimilarityMeasure, threshold: Double) =
    // bm: you just have to use this pattern, if u want create a AUXILIARY Constructor that
    // calles another method
    this(points, LinkMatrix.calculatePointSimilarities(points, pointSim), threshold)

  //bm: initialize class,
  init(this.points, this.similarityMatrix)

  private def init(points: Array[WordDataPoint],
                   similarityMatrix: Array[Array[Double]]): Unit = {

    points.foreach(objToIndexMapping.getIndex(_))
    val length = points.length

    // Identify neighbors: a[i][j] == 1 if (i,j) are neighbors and 0 otherwise.
    for (i <- 0 until length) {
      for (j <- (i + 1) until length) {
        if (similarityMatrix(i)(j) > threshold) {
          // greater than threshold.
          // why not less than threshold?, well the greater values means more similarity
          pointLinkMatrix(i)(j) = 1
        } else {
          pointLinkMatrix(i)(j) = 0
        }
        pointNeighborMatrix(j)(i) = pointNeighborMatrix(i)(j) // set symmetry side
      }
      pointNeighborMatrix(i)(i) = 1 // self vs self is 1
    }
    // Calculate number of links between points
    for (i <- 0 until length) {
      for (j <- i until length) {
        // note: this includes the point itself
        pointLinkMatrix(i)(j) = nLinksBetweenPoints(pointNeighborMatrix, i, j)
        pointLinkMatrix(j)(i) = pointLinkMatrix(i)(j)
      }
    }
  }

  /**
    * Calculates number of links between two clusters. Number of links between
    * two clusters is the sum of links between all point pairs( p1, p2) where
    * p1 belongs to the first cluster and p2 belongs to the other cluster.
    *
    * @param clusterX
    * @param clusterY
    * @return link count between two clusters.
    */
  def getLinks(clusterX: Cluster[WordDataPoint], clusterY: Cluster[WordDataPoint]): Int = {

    val itemsX = clusterX.getElements
    val itemsY = clusterY.getElements

    val links = for (x <- itemsX; y <- itemsY) yield getLinks(x, y)
    links.foldLeft(0)(_ + _)
  }

  def getLinks(p1: WordDataPoint, p2: WordDataPoint): Int = {

    val i = objToIndexMapping.getIndex(p1)
    val j = objToIndexMapping.getIndex(p2)
    return pointLinkMatrix(i)(j)
  }

  private def nLinksBetweenPoints(neighbors: Array[Array[Int]], idxX: Int, idxY: Int): Int =
    neighbors.zipWithIndex.foldLeft(0) { case (acc, (arr, idx)) =>
      acc + neighbors(idxX)(idx) * neighbors(idx)(idxY)
    }


  def printSimilarityMatrix(): Unit = {

    println("Point Similarity matrix:")
    similarityMatrix.foreach(util.Arrays.toString(_))
  }

  def printPointNeighborMatrix(): Unit = {

    println(s"Point Neighbor matrix (threshold=${this.threshold}):")
    pointNeighborMatrix.foreach(util.Arrays.toString(_))
  }

  def printPointLinkMatrix(): Unit = {

    println(s"Point Link matrix (threshold=${this.threshold}):")
    pointLinkMatrix.foreach(util.Arrays.toString(_))
  }
}

object LinkMatrix {
  def apply(points: Array[WordDataPoint], similarityMatrix: Array[Array[Double]],
            threshold: Double) = new LinkMatrix(points, similarityMatrix, threshold)

  def apply(points: Array[WordDataPoint], pointSim: SimilarityMeasure, threshold: Double) = {
    new LinkMatrix(points, calculatePointSimilarities(points, pointSim), threshold)
  }

  /*
   * Calculates similarity matrix for all points.
   */
  private def calculatePointSimilarities(points: Array[WordDataPoint],
                                 pointSim: SimilarityMeasure): Array[Array[Double]] = {

    val length = points.length
    val simMatrix = Array.fill(length, length)(0.0)

    for (i <- 0 until length) {
      val attrsX = points(i).values
      for (j <- (i + 1) until length) {
        val attrsY = points(j).values

        simMatrix(i)(j) = pointSim.similarity(attrsX, attrsY)
        simMatrix(j)(i) = simMatrix(i)(j)
      }
    }

    return simMatrix

  }
}

/**
  * Goodness measure for merging two clusters.
  */
// todo: method g ?, after figuring out what links means
class MergeGoodnessMeasure(linkThreshold: Double) {
  private val p: Double = 1.0 + 2.0 * f(linkThreshold);

  /**
    * This implementation assumes that linkThreshold was a threshold for
    * similarity measure (as opposed to dissimilarity/distance).
    *
    * @param threshold linkThreshold threshold value that was used to identify neighbors among points.
    * @return
    */
  def f(threshold: Double): Double = (1.0 - threshold) / (1.0 + threshold)

  /**
    * this formula is explained in book @ page 150
    *
    * @param nLinks
    * @param nX
    * @param nY
    * @return
    */
  def g(nLinks: Int, nX: Int, nY: Int): Double = {

    val a = Math.pow(nX + nY, p)
    val b = Math.pow(nX, p)
    val c = Math.pow(nY, p)

    return nLinks / (a - b - c)
  }

}

case class SimilarCluster(clusterKey: Int, goodness: Double) extends Ordered[SimilarCluster] {
  //"[clusterKey=" + this.clusterKey + ",goodness=" + this.goodness+ "]";
  override def toString: String = s"[clusterKey=${this.clusterKey}, goodness=${this.goodness}]"

  override def compare(that: SimilarCluster): Int = {

    if (this.goodness > that.goodness) {
      -1 // order in the decreasing order of goodness value
    } else if (this.goodness == that.goodness) {
      0
    } else {
      1
    }
  }
}
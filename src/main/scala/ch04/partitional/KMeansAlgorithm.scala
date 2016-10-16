/**
  * Created by interaction on 9/10/16.
  */
package iweb.ch04.similarity.partitional

import java.util

import iweb.ch04.models.{Cluster, NumericDataPoint}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  *
  * @param allCentroids starting values for the centroids of each cluster.
  * @param dataPoints
  */
class KMeansAlgorithm(
                       allCentroids: Array[NumericDataPoint],
                       dataPoints: Array[NumericDataPoint]) {
  private val k = allCentroids.length
  private val allClusters = new Array[Cluster[NumericDataPoint]](k)

  def this(k: Int, dataPoints: Array[NumericDataPoint]) {
    this(KMeansAlgorithm.pickInitialCentroids(k, dataPoints), dataPoints)
  }


  def cluster(): Unit = {
    var centroidsChanged = true

    while (centroidsChanged) {
      // Create a set points for each cluster
      val clusters = ListBuffer.fill(k)(mutable.HashSet.empty[NumericDataPoint])

      // Assign points to each set based on minimum distance from the centroids
      dataPoints.foreach { e =>
        val idx = findClosestCentroid(allCentroids, e)
        clusters(idx).add(e)
      }

      clusters.zipWithIndex.foreach { case (e: mutable.HashSet[NumericDataPoint], idx: Int) =>
        allClusters(idx) = Cluster[NumericDataPoint](s"cluster ${idx}", e)
      }

      centroidsChanged = false

      require(allClusters.length == clusters.length)
      // Calculate new cluster centroids, and check if any of the centroids has changed
      // this process has to converge, which will be for it to end this loop
      for (i <- 0 until allClusters.length) {
        if (clusters(i).size != 0) {
          val newCentroidValues = findCentroid(allClusters(i))
          val oldCentroidValues = allCentroids(i).values

          if (!util.Arrays.equals(oldCentroidValues, newCentroidValues)) {
            allCentroids(i) = new NumericDataPoint(allCentroids(i).label, newCentroidValues)
            centroidsChanged = true
          }
        } else {
          // keep mean unchanged if cluster has no elements.
        }
      }
    }
  }
  def print(): Unit ={
    println("Clusters:")
    allClusters.foreach( e=> println(e.toString))
  }

  def printMeans(): Unit ={
    println("Cluster Means: ")
    allCentroids.foreach( e=> println(e.toString))
  }

  def printAll(): Unit ={
    print()
    println("___________________________________________________")
    printMeans()
  }

  /**
    * This method calculates the closest centroid for a given data point
    *
    * @param centroids
    * @param x is the <CODE>DataPoint</CODE> for which we seek the closest centroid
    * @return the index (from the centroids array) of the closest centroid
    */
  private def findClosestCentroid(centroids: Array[NumericDataPoint], x: NumericDataPoint): Int = {
    var closedDistance = Double.PositiveInfinity
    var result = 0
    for (i <- centroids.indices) {
      val d = centroids(i)
      val ds = distance(d, x)
      if (closedDistance > ds) {
        closedDistance = ds
        result = i
      }
    }

    result
  }

  // return 在这个cluster中每个特征点的平均值
  private def findCentroid(c: Cluster[NumericDataPoint]): Array[Double] = {
    if (c.size == 0) return Array(0.0)

    val d = c.getDimensionCount
    var result = new Array[Double](d)

    c.elements.foreach { e =>
      for (i <- 0 until d){
        result(i) += e.values(i)
      }
    }

    result = result.map(_ / c.size)
    result
  }

  private def distance(x: NumericDataPoint, y: NumericDataPoint): Double = distance(x.values, y.values)

  private def distance(x: Array[Double], y: Array[Double]): Double = {
    val sum = x.zip(y).foldLeft(0.0)((acc, e) => acc + Math.pow(e._1 - e._2, 2))
    Math.sqrt(sum)
  }
}

object KMeansAlgorithm {


  // public static DataPoint[] pickInitialCentroids(int k, DataPoint[] data) {

  // @param k, the desired cluster size
  def pickInitialCentroids(k: Int, data: Array[NumericDataPoint]): Array[NumericDataPoint] = {

    // Calculate random mean values for each cluster based on the data
    /**
      * TODO: 4.2 -- Selecting the means for seeding
      *
      * In large datasets, the selection of the initial centroids can be
      * important from a computational (time) complexity perspective.
      *
      * In general, how can we improve the seeding of the initial mean values?
      * For example, consider the following heuristic:
      *
      * 1. pick randomly one node
      * 2. calculate the distance between that node and O (10*k) other nodes
      * 3. sort the list of nodes according to their distance from the first node
      * 4. pick every 10th node in the sequence
      * 5. calculate the mean distance between each one of these nodes and the original node
      *
      * This algorithmic choice is as ad hoc as they come, however, it does have
      * some key principles embedded in it? What are these principles?
      * How can you generalize this algorithm?
      *
      * Discuss advantages/disadvantages of the initial seeding with your friends.
      *
      */

    val random = new Random()
    val results = new Array[NumericDataPoint](k)
    val sets = mutable.HashSet.empty[NumericDataPoint]
    for (i <- 0 until k) {
      var idx = 0
      do {
        idx = random.nextInt(data.length)
      } while (sets.add(data(idx)) == false)

      results(idx) = data(idx).clone.copy(s"Mean-${i}(${data(idx).label})")
    }

    results
  }
}

object TestKMeansAlgorithm extends App {
  val dataPoints = Array(
    new NumericDataPoint("2", Array(2.0)),
    new NumericDataPoint("4", Array(4.0)),
    new NumericDataPoint("10", Array(10.0)),
    new NumericDataPoint("12", Array(12.0)),
    new NumericDataPoint("3", Array(3.0)),
    new NumericDataPoint("20", Array(20.0)),
    new NumericDataPoint("30", Array(30.0)),
    new NumericDataPoint("11", Array(11.0)),
    new NumericDataPoint("25", Array(25.0))
  )

  val clusterMeans = Array(
    new NumericDataPoint("Mean-2", Array(2.0)),
    new NumericDataPoint("Mean-4", Array(4.0))
  )

  val kmeans = new KMeansAlgorithm(clusterMeans, dataPoints)
  kmeans.cluster()
  kmeans.print()
}
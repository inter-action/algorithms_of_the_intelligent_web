package iweb.ch04.rock

import iweb.ch04.models.{Cluster, WordDataPoint}
import iweb.ch04.similarity.hierarchical.Dendrogram
import iweb.ch04.similarity.{JaccardCoefficient, SimilarityMeasure}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class ROCKAlgorithm(points: Array[WordDataPoint],
                    k: Int,
                    threshold: Double,
                    similarityMeasure: SimilarityMeasure) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  private val linkMatrix = new LinkMatrix(points, similarityMeasure, threshold)

  def cluster(): Dendrogram[WordDataPoint] = {

    val initialClusters: Seq[Cluster[WordDataPoint]] =
      points.map(e => new Cluster[WordDataPoint]("", Seq(e))).toSeq
    var g = Double.PositiveInfinity
    val dnd = new Dendrogram[WordDataPoint]("Goodness")
    dnd.addLevel(g.toString, initialClusters)
    val goodnessMeasure = new MergeGoodnessMeasure(this.threshold)
    val allClusters = new ROCKClusters(initialClusters.toList, linkMatrix, goodnessMeasure)
    var nClusters = allClusters.size
    var isBreak = false
    while (nClusters > k && !isBreak) {
      val nClustersBeforeMerge = nClusters
      g = allClusters.mergeBestCandidates()
      nClusters = allClusters.size
      if (nClusters == nClustersBeforeMerge) {
        isBreak = true
      } else {
        dnd.addLevel(g.toString, allClusters.getAllClusters())
      }
    }
    logger.info(s"Number of clusters: ${allClusters.getAllClusters().size}")
    dnd
  }
}


/**
  *
  * @param initialClusters
  * @param linkMatrix      Links between data points and clusters.
  * @param goodnessMeasure way of measure goodness
  */
class ROCKClusters(initialClusters: List[Cluster[WordDataPoint]],
                   linkMatrix: LinkMatrix,
                   goodnessMeasure: MergeGoodnessMeasure) {

  // Provides ID -> Cluster mapping.
  private val clusterMap: mutable.Map[Int, Cluster[WordDataPoint]] =
    mutable.Map.empty[Int, Cluster[WordDataPoint]] ++= (initialClusters.zipWithIndex.map({ e => (e._2, e._1) }).toMap)
  private var key = clusterMap.size
  /*
   * Provides ID -> Similar Clusters mapping.
   * List<SimilarCluster> 是跟index为key的cluster节点相似度的cluster列表,
   * 相似度以SimilarCluster中的goodness计算的
   */
  private val similarClustersMap = mutable.Map.empty[Int, List[SimilarCluster]]

  calculateClusterSimilarities()

  def calculateClusterSimilarities(): Unit = {

    similarClustersMap.clear()
    for ((k, v) <- clusterMap) {
      val ls = ListBuffer.empty[SimilarCluster]

      for ((ik, iv) <- clusterMap) {
        val links = linkMatrix.getLinks(v, iv)
        if (links > 0) {
          val goodness = this.goodnessMeasure.g(links, v.size, iv.size)
          ls += new SimilarCluster(ik, goodness)
        }
      }

      similarClustersMap += k -> ls.toList.sorted
    }
  }

  def size: Int = clusterMap.size

  /**
    * merge two best candidates
    *
    * @return Double.NaN if no candiates are merged
    */
  def mergeBestCandidates(): Double = {

    val candidates = findBestMergeCandidates()

    var goodness = Double.NaN
    if (candidates.isDefined) {
      val (clusterKey1, clusterKey2) = candidates.get
      mergeClusters(clusterKey1, clusterKey2)
      val cls = similarClustersMap.get(clusterKey1).get
      goodness = cls.head.goodness
    }

    goodness
  }

  // Finds a pair of cluster indexes with the best goodness measure.
  def findBestMergeCandidates(): Option[(Int, Int)] = {

    var bestKey: Int = -1
    var bestSimilarCluster: SimilarCluster = null
    var bestGoodness = Double.NegativeInfinity

    for ((k, similarityCluster) <- similarClustersMap) {
      if (similarityCluster.nonEmpty) {
        val topGoodness = similarityCluster.head.goodness
        if (topGoodness > bestGoodness) {
          bestKey = k
          bestSimilarCluster = similarityCluster.head
          bestGoodness = topGoodness
        }
      }
    }

    if (bestKey != -1) {
      Some((bestKey, bestSimilarCluster.clusterKey))
    } else {
      None
    }
  }


  def mergeClusters(key1: Int,
                    key2: Int): Int = {

    val c1 = clusterMap.get(key1).get
    val c2 = clusterMap.get(key2).get
    val c3 = new Cluster(c1, c2)
    clusterMap.-=(key1, key2) // Removes two or more elements from this shrinkable collection.
    clusterMap += key -> c3
    key += 1
    calculateClusterSimilarities()
    key - 1
  }

  def getAllClusters(): Seq[Cluster[WordDataPoint]] = clusterMap.toSeq.map(_._2)
}


object TestROCKAlgorithm extends App {
  val elements = new Array[WordDataPoint](4)
  elements(0) = new WordDataPoint("Doc1", Array("book"))
  elements(1) = new WordDataPoint("Doc2", Array("water", "sun", "sand", "swim"))
  elements(2) = new WordDataPoint("Doc3", Array("water", "sun", "swim", "read"))
  elements(3) = new WordDataPoint("Doc4", Array("read", "sand"))

  val rock = new ROCKAlgorithm(elements, 1, 0.2, JaccardCoefficient)
  val dnd = rock.cluster()
  dnd.printAll()
}
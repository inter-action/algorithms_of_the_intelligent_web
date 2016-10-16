package iweb.ch04.similarity.hierarchical

import iweb.ch04.models.{Cluster, DataPoint}
import org.slf4j.LoggerFactory

import scala.collection.mutable


class ClusterSet[U <: DataPoint] {
  type C = Cluster[U]

  private val allClusters: mutable.Set[C] = mutable.Set.empty[C]

  def findClusterByElement(e: U): Option[C] = allClusters.find( _.contains(e) )

  def getAllCluster: mutable.Set[C] = allClusters.map(_.clone())

  def add(c: C): Unit = allClusters.add(c)

  def add(d: U): Unit = {
    val c = new C
    c.add(d)
    add(c)
  }

  def remove(c: C) = allClusters.remove(c)

  def size:Int = allClusters.size
}


class Dendrogram[U <: DataPoint](levelLabelName: String) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  
  private val entryMap = mutable.Map.empty[Int, ClusterSet[U]]
  private val labelMap = mutable.Map.empty[Int, String]
  private var nextLevel = 1

  def addLevel(label: String, cluster: Cluster[U]): Int = addLevel(label, Seq(cluster))


  /**
    * Creates a new dendrogram level using copies of provided clusters.
  */
  def addLevel(label: String, clusters: Seq[Cluster[U]]) = {
    val clusterSet = new ClusterSet[U]

    clusters.foreach( clusterSet.add(_) )

    val level = nextLevel
    entryMap += level -> clusterSet
    labelMap += level -> label

    nextLevel += 1

    level
  }

  /**
  * Replaces clusters in the specified level. If level doesn't exist it will
  * be created.
  *
  * @param level dendrogram level.
  * @param label level description.
  * @param clusters clusters for the level.
  * @return
  */
  def setLevel(level: Int, label: String, clusters: Seq[Cluster[U]]): Unit ={
    val clusterSet = new ClusterSet[U]

    clusters.foreach(e=> clusterSet.add( e.clone() ) )

    entryMap.put(level, clusterSet)
    labelMap.put(level, label)

    if (level >= nextLevel) nextLevel = level + 1
  }

  def getTopLevel = nextLevel - 1

  def getAllLevels: Seq[Int] = entryMap.keySet.toSeq

  def getLabelForLevel(level: Int): Option[String] = labelMap.get(level)

  def getClustersForLevel(level: Int): Option[Seq[Cluster[U]]] =
    entryMap.get(level).map(_.getAllCluster.toSeq)

  def print(level: Int): Unit ={
    (labelMap.get(level), entryMap.get(level)) match {
      case (Some(label), Some(clusters)) =>
        logger.info(s"Clusters for: level=${level}, levelLabelName=${levelLabelName}, label=${label}")
        var i = 0
        for (c <- clusters.getAllCluster if c.getElements.size > 1){
          logger.info(s"start: Cluster No.${i}------------------------\n")
          logger.info(c.toString)
          logger.info(s"end: Cluster No.${i}------------------------\n")
          i += 1
        }
      case _ =>
        logger.info( s"no elements found for this level : ${level}")
    }
  }

  def printAll(): Unit ={
    // print with order
    entryMap.toSeq.sortBy( _._1 ).foreach(e=> print(e._1) )
  }

}
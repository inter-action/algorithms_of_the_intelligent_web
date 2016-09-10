package iweb.ch04.similarity.hierarchical

import iweb.ch04.models.{DataPoint, Cluster}

import scala.collection.mutable


class ClusterSet[T, U <: DataPoint[T]] {
  type C = Cluster[T, U]

  private val allClusters: mutable.Set[C] = mutable.Set.empty[C]

  def findClusterByElement(e: U): Option[C] = allClusters.find( _.contains(e) )

  def getAllCluster: mutable.Set[C] = allClusters.map(_.clone())

  def add(c: C) = allClusters.add(c)

  def remove(c: C) = allClusters.remove(c)

  def size:Int = allClusters.size
}


class Dendrogram[T, U <: DataPoint[T]](levelLabelName: String) {
  private val entryMap = mutable.Map.empty[Int, ClusterSet[T, U]]
  private val labelMap = mutable.Map.empty[Int, String]
  private var nextLevel = 1

  def addLevel(label: String, cluster: Cluster[T, U]): Int = addLevel(label, Seq(cluster))


  /**
    * Creates a new dendrogram level using copies of provided clusters.
  */
  def addLevel(label: String, clusters: Seq[Cluster[T, U]]) = {
    val clusterSet = new ClusterSet[T, U]

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
  def setLevel(level: Int, label: String, clusters: Seq[Cluster[T, U]]): Unit ={
    val clusterSet = new ClusterSet[T, U]

    clusters.foreach(e=> clusterSet.add( e.clone() ) )

    entryMap.put(level, clusterSet)
    labelMap.put(level, label)

    if (level >= nextLevel) nextLevel = level + 1
  }

  def getTopLevel = nextLevel - 1

  def getAllLevels: Seq[Int] = entryMap.keySet.toSeq

  def getLabelForLevel(level: Int): Option[String] = labelMap.get(level)

  def getClustersForLevel(level: Int): Option[Seq[Cluster[T, U]]] =
    entryMap.get(level).map(_.getAllCluster.toSeq)

  //_.getAllCluster.toSeq

  def print(level: Int): Unit ={
    (labelMap.get(level), entryMap.get(level)) match {
      case (Some(label), Some(clusters)) =>
        println(s"Clusters for: level=${level}, levelLabelName=${levelLabelName}, label=${label}")
        for (c <- clusters.getAllCluster if c.getElements.size > 1){
          println("------------------------\n")
          println(c.toString)
          println("------------------------\n\n")
        }
      case _ =>
        println( s"no elements found for this level : ${level}")
    }
  }

  def printAll(): Unit ={
    // print with order
    entryMap.toSeq.sortBy( _._1 ).foreach(e=> print(e._1) )
  }

}
package iweb.ch04.similarity.hierarchical

import iweb.ch04.models.{Attribute, Cluster, NumericDataPoint}
import iweb.ch04.utils.ObjectToIndexMapping

class AverageLinkAlgorithm(
                            elements: Array[NumericDataPoint],
                            adjacencyMatrix: Array[Array[Double]]) {

  private val allClusters = new ClusterSet[NumericDataPoint]()



  def cluster(): Dendrogram[NumericDataPoint] = {
    val dnd = new Dendrogram[NumericDataPoint]("Distance")
    var d = 0.0

    elements.foreach { e=>
      val c = new Cluster[NumericDataPoint]()
      c.add(e)
      allClusters.add(c)
    }

    dnd.addLevel(d.toString, allClusters.getAllCluster.toSeq)

    d = 1.0

    while(allClusters.size > 1){
      val K = allClusters.size
      mergeCluster(d)
      if (K > allClusters.size){
        dnd.addLevel(d.toString, allClusters.getAllCluster.toSeq)
      }

      d += 0.5
    }


    dnd
  }


  private def mergeCluster(distanceThreshold: Double): Unit ={

    val idxMapping = new ObjectToIndexMapping[Cluster[NumericDataPoint]]
    val alength = adjacencyMatrix.length

    val nClusters = allClusters.size
    val clusterDistances: Array[Array[Double]] = Array.fill(nClusters, nClusters)(0.0)


    for (i <- 0 until alength; j <- (i+1) until alength){
      val d = adjacencyMatrix(i)(j)

      if (d > 0){
        val ei = elements(i)
        val ej = elements(j)
        val ci = allClusters.findClusterByElement(ei).get // throw error if not found
        val cj = allClusters.findClusterByElement(ej).get

        if (ci != cj){
          val idxI = idxMapping.getIndex(ci)
          val idxJ = idxMapping.getIndex(cj)
          //accumulate similarity value
          clusterDistances(idxI)(idxJ) += d
          clusterDistances(idxJ)(idxI) += d
        }
      }
    }


    val merged = Array.fill(nClusters)(false)
    for (i <- 0 until nClusters; j <- (i+1) until nClusters){
      val ci = idxMapping.getObject(i).get
      val cj = idxMapping.getObject(j).get

      val value = clusterDistances(i)(j) / ( ci.size * cj.size ) // why multiply here ?
      clusterDistances(i)(j) = value
      clusterDistances(j)(i) = value

      if (merged(i) == false && merged(j) == false){
        if (clusterDistances(i)(j) <= distanceThreshold){
          allClusters.remove(ci)
          allClusters.remove(cj)

          val _new = new Cluster[NumericDataPoint](ci, cj)
          allClusters.add(_new)
          merged(i) = true
          merged(j) = true
        }
      }
    }
  }

}

object TestAverageLinkAlgorithm extends App{

  val elements = new Array[NumericDataPoint](5)
  elements(0) = new NumericDataPoint("A", Array.empty[Attribute[Double]])
  elements(1) = new NumericDataPoint("B", Array.empty[Attribute[Double]])
  elements(2) = new NumericDataPoint("C", Array.empty[Attribute[Double]])
  elements(3) = new NumericDataPoint("D", Array.empty[Attribute[Double]])
  elements(4) = new NumericDataPoint("E", Array.empty[Attribute[Double]])


  val ajs = Array(
    Array[Double](0.0, 1.0, 2.0, 2.0, 3.0),
    Array[Double](1d, 0d, 2d, 4d, 3),
    Array[Double](2d, 2d, 0d, 1d, 5d),
    Array[Double](2d, 4d, 1d, 0d, 3d),
    Array[Double](3d, 3d, 5d, 3d, 0d)
  )

  val ca = new AverageLinkAlgorithm(elements, ajs)
  val dnd = ca.cluster()
  dnd.printAll()

}
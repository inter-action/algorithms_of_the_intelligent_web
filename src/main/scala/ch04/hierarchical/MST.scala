package iweb.ch04.similarity.hierarchical

import iweb.ch04.models.{Attribute, Cluster, NumbericDataPoint}


class MSTSingleLinkAlgorithm(
                            elements: Array[NumbericDataPoint],
                            adjacencyMatrix: Array[Array[Double]]
                            ){
  private val allClusters = new ClusterSet[Double, NumbericDataPoint]()
  private var mst: Array[Array[Double]] = null


  def cluster(): Dendrogram[Double, NumbericDataPoint] ={
    mst = MST.buildMST(adjacencyMatrix)

    elements.foreach( allClusters.add(_) )
    val dnd = new Dendrogram[Double, NumbericDataPoint]("Distance")
    var d = 0.0
    var lastDndLevel = dnd.addLevel(d.toString, allClusters.getAllCluster.toSeq)
    var previousD = d

    while( allClusters.size > 1){
      d = mergeTwoClosestClusters()

      if (previousD == d){
        dnd.setLevel(lastDndLevel, d.toString, allClusters.getAllCluster.toSeq)
      }else{
        lastDndLevel = dnd.addLevel(d.toString, allClusters.getAllCluster.toSeq)
      }
      previousD = d
    }

    dnd
  }


  private def mergeTwoClosestClusters(): Double = {
    var minI = -1
    var minJ = -1
    var minWeight = Double.PositiveInfinity
    require(mst.length == elements.length && elements.length == adjacencyMatrix.length)
    val length = elements.length

    // find closet element
    for (i <- 0 until length; j <- 0 until length){
      if (minWeight > mst(i)(j) && mst(i)(j) >= 0){
        minWeight = mst(i)(j)
        minI = i
        minJ = j
      }
    }

    var result = Double.NaN

    if (minI > -1){
      val c1 = allClusters.findClusterByElement(elements(minI)).get
      val c2 = allClusters.findClusterByElement(elements(minJ)).get
      allClusters.remove(c1)
      allClusters.remove(c2)
      allClusters.add(new Cluster[Double, NumbericDataPoint](c1, c2))
      result = minWeight
      // remove link. Using -1 because 0 is a valid distance.
      mst(minI)(minJ) = -1
      mst(minJ)(minI) = -1
    }

    result
  }


}



// Basic implementation of Prim's algorithm to build Minimal Spanning Tree (MST).
object MST {

  /**
    *
    * @param adjacencyMatrix The adjacency matrix of the graph
    * @return
    */
  def buildMST(adjacencyMatrix: Array[Array[Double]]): Array[Array[Double]] = {
    val mLength = adjacencyMatrix.length

    // Marks nodes that belong to MST. Initial MST has only one node.


    val mstV = Array.fill(mLength)(false)
    mstV(0) = true

    /*
    Using -1 to indicate that there is no edge between nodes i and j.
    Can't use 0 because it is a valid distance.
    */
    val result = Array.fill(mLength, mLength)(-1.0)

    var t = findMinimumEdge(adjacencyMatrix, mstV)
    while( !t.isEmpty ){
      val edge = t.get
      mstV(edge.j) = true
      result(edge.i)(edge.j) = edge.w
      result(edge.j)(edge.i) =edge.w
      t = findMinimumEdge(adjacencyMatrix, mstV)
    }

    result
  }


  private def findMinimumEdge(
                               adjacencyMatrix: Array[Array[Double]],
                               mstV: Array[Boolean]
                             ): Option[Edge] = {

    var temp = Double.PositiveInfinity
    val length = adjacencyMatrix.length
    var result = Option.empty[Edge]

    for (i <- 0 until length if mstV(i)) { // part of MST
      for (j <- 0 until length if !mstV(j)) { // not part of MST
        val w = adjacencyMatrix(i)(j)
        if (temp > w) {
          temp = w
          result = Some(Edge(i, j, w))
        }
      }
    }

    result
  }

  /*
  Graph Edge, i: Matrix row, j: Matric column, w: similiarity value
   */
  private case class Edge(i: Int, j: Int, w: Double)

}


object TestMSTSingleLinkAlgorithm extends App{
  val elements = new Array[NumbericDataPoint](5)
  elements(0) = new NumbericDataPoint("A", Array.empty[Attribute[Double]])
  elements(1) = new NumbericDataPoint("B", Array.empty[Attribute[Double]])
  elements(2) = new NumbericDataPoint("C", Array.empty[Attribute[Double]])
  elements(3) = new NumbericDataPoint("D", Array.empty[Attribute[Double]])
  elements(4) = new NumbericDataPoint("E", Array.empty[Attribute[Double]])


  val ajs = Array(
    Array[Double](0.0, 1.0, 2.0, 2.0, 3.0),
    Array[Double](1d, 0d, 2d, 4d, 3),
    Array[Double](2d, 2d, 0d, 1d, 5d),
    Array[Double](2d, 4d, 1d, 0d, 3d),
    Array[Double](3d, 3d, 5d, 3d, 0d)
  )

  val ca = new MSTSingleLinkAlgorithm(elements, ajs)
  val dnd = ca.cluster()
  dnd.printAll()
}



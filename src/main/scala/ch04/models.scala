package iweb.ch04.models

import iweb.ch04.similarity.EuclideanDistance

import scala.collection.mutable


case class Attribute[T](name: String, value: T)

object Attribute{
  def createAttributes[T](attrValues: Array[T]): Array[Attribute[T]] =
//    for (idx <- attrValues.indices) yield Attribute("a-"+idx, attrValues(idx))
    attrValues.zipWithIndex.map{ case (t, idx) => Attribute("a-"+idx, t)}


  def getNames[T](attrs: Array[Attribute[T]]): Array[String] = attrs.map(_.name)

  def getValues[T](attrs: Array[Attribute[T]]): Array[T] = attrs.map(_.value)
}


trait DataPoint[T]{
  val label: String
  val attrs: Array[Attribute[T]]
  val attrNames: Array[String] = Attribute.getNames(attrs)
  val values: Array[T] = Attribute.getValues(attrs)

  def getR: Double

  def getAttributeCount:Int = attrs.length
  // only print values of attrs
  def toShortString = s"${label} ( ${attrs.map(_.value.toString).mkString("\n")} )"
  override def toString = s"${label} ( ${attrs.map(_.toString()).mkString("\n")} )"
}

case class NumbericDataPoint(label: String, attrs: Array[Attribute[Double]]) extends DataPoint[Double]{
  override def getR: Double = EuclideanDistance.getDistance(new Array[Double](attrs.length), values)

}



// todo: whether change elements to immutable objects
case class Cluster(
                    label: String = "",
                    elements: scala.collection.Set[DataPoint] = mutable.LinkedHashSet.empty[DataPoint]){


  def this(label: String, elements: Seq[DataPoint]){
    this(label, mutable.LinkedHashSet.empty[DataPoint] ++ elements)
  }

  def this(c1: Cluster, c2: Cluster){
    this("", c1.elements ++ c2.elements)
  }

  def this(c1: Cluster*){
    this("",
      c1.foldLeft(mutable.LinkedHashSet.empty[DataPoint]){ (acc, c) =>
        acc ++ c.elements
      }
    )
  }

  def size = elements.size

  def getDimensionCount = if (elements.isEmpty) 0 else elements.iterator.next().getAttributeCount

  // def copy: Cluster ?, elements need to be copied?

  def add(cluster: Cluster):Unit = {
    elements ++= cluster.elements
  }

  def add(e: DataPoint): Unit ={
    elements += e
  }

  def contains(c: Cluster) = elements.intersect(c.elements).size != 0

  def contains(e: DataPoint) = elements.contains(e)

  def getElements: mutable.Set[DataPoint] = mutable.LinkedHashSet.empty[DataPoint] ++ elements

  override def toString = s"${elements.mkString(",\n")} \n ${label}"

}
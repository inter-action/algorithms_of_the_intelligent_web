# About This Repo
I gonna re-write some code at [https://github.com/CrazyFork/yooreeka](https://github.com/CrazyFork/yooreeka)
with scala as means to help me understand more of the algorithms in that book(*the algorithms of intelligent web*)
also be more familiar with scala language.

the work starts from chapter 04.



### scala traphole


Generic Array with Map return ArraySeq

    // this just suggest using Array in scala is pretty much bad idea
    // http://stackoverflow.com/questions/12837799/scala-array-map-returns-arrayseq
    // painful as fuck !, had to hate scala this way
    def getValues[T: ClassTag](attrs: Array[Attribute[T]]): Array[T] = attrs.map(_.value)



### Notes

* in chapter 04's code, the container i used basically are mutable, maybe i should use immutable
container instead, could save me lots of headache of copying things




### Pain in the ass working with scala types :(

    // can `U <: DataPoint[T]` not be propage to outer user/enclosing class ?
    trait DataPoint[T]{

    case class Cluster[T, U <: DataPoint[T]](

    class ClusterSet[T, U <: DataPoint[T]] {
      type C = Cluster[T, U]


    // also if you write this, compile gonna failed, you will be warned by compiler
    Cluster("test-1", elements) 
    
    // instead you have to tell complier more about types
    Cluster[Double, NumbericDataPoint] ("test-1", elements)







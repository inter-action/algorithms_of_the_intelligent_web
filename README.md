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



val inside this constructor:

    class KMeansAlgorithm(
                           allCentroids: Array[NumericDataPoint],
                           dataPoints: Array[NumericDataPoint]){
      private val k = allCentroids.length
      private val allClusters = new Array[Cluster[Double, NumericDataPoint]](k)
      
      def this(k: Int, dataPoints: Array[NumericDataPoint]){
        // note this would not compile, 
        val initialCentroids = KMeansAlgorithm.pickInitialCentroids(k, dataPoints)
        this(initialCentroids, dataPoints)
      }
        
      //instead you write
      def this(k: Int, dataPoints: Array[NumericDataPoint]){
        this(KMeansAlgorithm.pickInitialCentroids(k, dataPoints), dataPoints)
      }

### Notes

* in chapter 04's code, the container i used basically are mutable, maybe i should use immutable
container instead, could save me lots of headache of copying things




### Pain in the ass working with scala types :(

had to say i this problem existed due to my wrong implementation :(

    // can `U <: DataPoint[T]` not bubble to outer user/enclosing class ?
    trait DataPoint[T]{
    
    case class NumericDataPoint(label: String, attrs: Array[Attribute[Double]]) extends DataPoint[Double]{
    
    /*
    since we want pass NumericDataPoint to this cluster, I have to specify 
    `Double` type as `T` then `NumericDataPoint` type as `U`, which is of course kind of redudant,
    case NumericDataPoint implemented with `Double` type, we expect the compiler to infer that out
     */
    case class Cluster[T, U <: DataPoint[T]](

    class ClusterSet[T, U <: DataPoint[T]] {
      type C = Cluster[T, U]


    // also if you write this, compile gonna failed, you will be warned by compiler
    Cluster("test-1", elements) 
    
    // instead you have to tell complier more about types
    Cluster[Double, NumbericDataPoint] ("test-1", elements)

now I change it to: 
@git sha `be1c7a023c5343598855114015da49e69f65c108`
    
    // we move type T inide trait body
    trait DataPoint{
      type T
    
    case class NumericDataPoint(label: String, attrs: Array[Attribute[Double]]) extends DataPoint{
      type T = Double
    
    // now we remove T type
    case class Cluster[U <: DataPoint](
    
    // etc ...

this shows Specify type on Trait or Abstract class may not be a good practice.





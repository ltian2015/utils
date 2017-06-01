package akkaLearn.akkalean

/**
 * @author ${user.name}
 */
object Test extends  App{ 
  class Duration(val count:Long,val unit:String)
  
  def foo(x : Array[String]) = x.foldLeft("")((a,b) => a + b)
  def test(prefix:String,d:Duration)=
  {
    println(prefix+" "+d.count+" "+d.unit);
  }
 // implicit def conveter(count:Long)=new TimeCount(count);
 implicit  class TimeCountConverter( count:Long){
     def seconds=new Duration(count,"second");
  }
 implicit object TimeCount{
   def seconds=new Duration(1,"Sring");
 }
  def seconds(count:Long)=new Duration(count,"second");
  def printDuration(d:Duration)={println(d.count+" "+d.unit)}
  private def safeMul(_a: Long, _b: Long): Long = {
    val a = scala.math.abs(_a)
    val b = scala.math.abs(_b)
    import java.lang.Long.{ numberOfLeadingZeros => leading }
    if (leading(a) + leading(b) < 64) throw new IllegalArgumentException("multiplication overflow")
    val product = a * b
    if (product < 0) throw new IllegalArgumentException("multiplication overflow")
    if (a == _a ^ b == _b) -product else product
  } 
  
    println( "Hello World!" )
    println("concat arguments = " + foo(args))
   // import scala.language.postfixOps;
    test("hello ",5 seconds);
    println(safeMul(-21223L,312121121211221L))
}

package domain.sample

object Main extends App {
  val fruitList=List("apple","orange","banana");
  val indexList=List(0,1,3,4);
  val tuples=fruitList.zipAll(indexList,"waht?" ,3);
  println(tuples);
  
  /* Predf中定义的???如下
   * def ??? : Nothing = throw new NotImplementedError
   */
 
 import scala.reflect.runtime.universe._
 
   def paramInfo[T: TypeTag](x: T): Unit = {
     val targs = typeOf[T] match { case TypeRef(_, _, args) => args }
      println(s"type of $x has type arguments $targs")
   }
 paramInfo(3);
 paramInfo(List(1));
 val m=Map(1->"cat",2->"dog");
 paramInfo(m);
 val c1=new C;
 c1.info;
}
class C{
  private  var h:Int= _;
  def info={
      val c2=new  C;
  c2.h=25;
  println(c2.h);
  }
}
class Time{
  private[this] var h=12;
  private[this] var m=12;
  def hour:Int=h;
  def hour_(x:Int)={
    require(0<=x && x<24)
    h=x;
  }
  def minute=m;
  def minute_(x:Int)={
    require(0<=x && x<60)
    m=x;
  }
}

 
package market.common.time.test

object LazyValTest extends App {
  private lazy val foo:Foo={
    println("hello for access foo");
    new Foo("1","apple");
  }
  println(foo);
  println(foo);
  println(foo);
  println(foo);
}
class Foo(val id:String,name:String){
  println("created (id="+id+",name="+name+")");
  override def toString="(id="+id+",name="+name+")";
};
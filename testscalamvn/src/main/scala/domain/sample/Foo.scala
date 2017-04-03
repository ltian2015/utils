package domain.sample

trait Foo {
   def id:String;
   def name:String;
   def description:String;
   def counts:Int;
   def div(share:Int):List[Foo];
   def toXML=
       <Foo id={id} name={name} description={description} />
}
object Foo{
  def apply( id:String, name:String, counts:Int,description:String=""):Foo={
    new MyFoo(id,name,counts,description);
  }
  private case class MyFoo(val id:String,val name:String,val counts:Int, description:String="") extends Foo{
    def div(share:Int):List[Foo]={
      require(share>1);
    val fooRange= for{i<- 1 to share;
          foo=Foo(this.id,this.name,this.counts /share,this.description)
      } yield foo;
     fooRange.toList;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////

package test.samples

import scala.reflect.runtime._
import scala.reflect.runtime.universe._
import ops.market.common.BaseTradeCenter
import ops.market.common.TradeCenter
import ops.market.common.RealTradeCenter
import ops.common.IdentifiedObject
import scala.collection.immutable.List._
import ops.common.Name
import ops.common.NameType
import ops.common.NameManageService
import ops.common.NameTypeAuthority
import ops.common.UndefinedNameTypeAuthority
import ops.common.NoAuthorityNameType
class CommonNameType extends NameType{
   def name:String="常用名"
   def description="常用的名字";
   def authority:NameTypeAuthority=UndefinedNameTypeAuthority;
}
 class TypedName(val name:String,val fieldPath:String) extends Name{
   def nameType:NameType=NoAuthorityNameType;
   override def toString=s"[TypedName=name: $name ; path: $fieldPath]" ;
}

object InstanceMirrorApp extends App {
 
 
 val btc:TradeCenter=new RealTradeCenter("100","sichuan","dff");
 def  typedNameGenerator(name:String,path:String):Name=new TypedName(name,path);
 implicit  val defaultTypedNameGenerator:(String,String)=>Name=typedNameGenerator;
  btc.printMetaInfo;
 //btc.printTypeInfo;
 println("************************************************************");
 //println(btc.getAnnotationMap);
 /**
 for (field<-btc.getGetterFields)
 {
   
   val fl:List[Char]= List.fill(15)(' ');
   val info:String="field:"+field.name.decodedName.toString().toList.zipAll(fl, ' ',' ').unzip._1.mkString+
                   " isMethod:"+field.isMethod+
                   " isSynthetic: "+field.isSynthetic+
                   " isVal:"+field.isVal+
                   " isVar:"+field.isVar+
                   " isAbstract:"+field.isAbstract+ 
                   " isAbstractOverride: "+field.isAbstractOverride+
                   " isImplementationArtifact:"+field.isImplementationArtifact+
                   " isJavaAnnotation:"+field.isJavaAnnotation+
                   " isOverloaded:"+field.isOverloaded+
                   " isPublic:"+field.isPublic+
                   " isStable:"+field.isStable+
                   " annotations:"+field.annotations
   println(info)
 }
 println("--------------------------------")
 println(btc.getNameGetters);
 * 
 */
 val m=currentMirror;
 val im:InstanceMirror =m.reflect(btc);
 val v:Any=im.instance;
 println(btc==v);//  true.
  val clsSbl:ClassSymbol=im.symbol;
  println(clsSbl);
  println(clsSbl.fullName);
  println(clsSbl.owner);
  println(clsSbl.overrides);
  val tpe:Type=clsSbl.typeSignature;
  println(tpe);
  println("------------------------tpe.members--------------------");
  println(tpe.members)
  println("------------------------tpe.decls--------------------");
  println(tpe.decls)
   println("------------------------tpe.dealias--------------------");
  val details:Type=tpe.dealias;
  println(details)
  println("------------------------tpe.typeSymbol--------------------");
  println(tpe.typeSymbol.fullName)
  println("------------------------tpe.typeSymbol--------------------");
  def getAnnotationMembers[T<:Any](obj:T):List[Symbol]={
    import scala.reflect.runtime.universe._
    val crtmr=currentMirror;
    val instmr:InstanceMirror =crtmr.reflect(this);
    Nil;
  }
  /**
 val l=for {s<-t.baseClasses;
    map=s.typeSignature.decls.collect { case m:Symbol=>m }.withFilter {
      _.annotations.length > 0
    }.map { m =>
      m.name.toString -> m.annotations.map { a =>
        a.tree.tpe.typeSymbol.name.toString -> a.tree.children.withFilter {
         _.productPrefix eq "AssignOrNamedArg"
        }.map { tree =>
          tree.productElement(0).toString -> tree.productElement(1)
        }.toMap
      }.toMap
    }.toMap
  } yield  map;
  println(l)
  **/
}
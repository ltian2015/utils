package test.samples

import scala.reflect.runtime._
import scala.reflect.runtime.universe._
import oem.market.common.BaseTradeCenter
import oem.market.common.TradeCenter
import oem.market.common.RealTradeCenter
import oem.market.common.IdentifiedObject
import scala.collection.immutable.List._
import oem.market.common.Name
import oem.market.common.NameType
import oem.market.common.NameManageService
import oem.market.common.NameTypeAuthority
import oem.market.common.UndefinedNameTypeManageService
import oem.market.common.UndefinedNameTypeAuthority
import oem.market.common.UndefinedNameType
class CommonNameType extends NameType{
   def name:String="常用名"
   def description="常用的名字";
   def authority:NameTypeAuthority=UndefinedNameTypeAuthority;
}
 

object InstanceMirrorApp extends App {
 class TypedName(val name:String,val fieldPath:String) extends Name{
   def nameType:NameType=UndefinedNameType;
   override def toString=s"[TypedName=name: $name ; path: $fieldPath]" ;
}
 
 val btc:TradeCenter=new RealTradeCenter("100","sichuan","dff");
 def  typedNameGenerator(name:String,path:String):Name=new TypedName(name,path);
 implicit  val defaultTypedNameGenerator:(String,String)=>Name=typedNameGenerator;
 println(btc.allNames);
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
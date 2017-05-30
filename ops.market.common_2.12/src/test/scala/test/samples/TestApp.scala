package test.samples
import scala.annotation._;
import scala.collection.mutable.ListBuffer;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import scala.reflect.api._
import scala.reflect.api.Universe
import ops.market.common.BaseTradeCenter
import ops.market.common.TradeCenter
import ops.common.IdentifiedObject
import scala.reflect.runtime.{universe=>ru}
import scala.reflect.runtime._
import scala.reflect.runtime.universe._

object AnnotationUtil
{
   /**
    * Returns a `Map` from method names to a `Map` from annotation names to
    * annotation data for the specified type.
    *
    * @tparam T The type to get method annotations for.
    * @return The method annotations for `T`.
    */
  
  def methodAnnotations[T: TypeTag]: Map[String, Map[String, Map[String, Any]]] = {
    typeOf[T].decls.collect { case m: MethodSymbol  => m }.withFilter {
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
  }
  
    def fieldAnnotations[T: TypeTag]: Map[String, Map[String, Map[String, Any]]] = {
    typeOf[T].decls.collect { case m:Symbol=>m }. withFilter {
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
  }
    val btc=new BaseTradeCenter("100","sichuan","dff");
 import scala.reflect.runtime._
 val m=currentMirror;
 val im:InstanceMirror =m.reflect(btc);
 val t=im.symbol;
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
     
  def getTypeTag[T](obj:T)(implicit ttg:TypeTag[T])=ttg;
  def getType[T:TypeTag](x:T):ru.Type=typeOf[T];
}
object TestApp extends App {
     val btc=new BaseTradeCenter("100","sichuan","dff");
 import scala.reflect.runtime._
 val m=currentMirror;
 val im:InstanceMirror =m.reflect(btc);
 val t=im.symbol;
 val l=for {s<-t.baseClasses;
    map=s.typeSignature.members.collect { case m:Symbol=>m }.withFilter {
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
 

 
}
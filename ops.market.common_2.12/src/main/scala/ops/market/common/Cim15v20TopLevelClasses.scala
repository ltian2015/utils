package ops
package market
package common
import java.time.LocalDateTime;
import ops.market.common.annocation._
import scala.collection.mutable.ListBuffer

/**
 * @author lantian
 */

trait IdentifiedObject 
{
  
   def mRID:String;
   
   @NameField
   def name:String;
   
   @NameField("别名") 
   def aliasName:String;
   
   private[this] def noTypedNameGenerator(name:String,fieldPath:String):Name=new NoTypedName(name,fieldPath);
  
   def allNames(implicit nameGenerator:(String,String)=>Name=noTypedNameGenerator):List[Name]={
    
     val names:ListBuffer[Name]=ListBuffer();
     for(i<-1 to 5){
       names+=nameGenerator(s"name$i",s"path:$i")
     }
     names.toList; 
     
   }
  
 
   import scala.reflect.runtime._
   import scala.reflect.runtime.universe._
   private[this] def getNames:List[Name]= {
     import scala.reflect.runtime.universe._
    val crtmr=currentMirror;
    val instmr:InstanceMirror =crtmr.reflect(this);
    val sybl=instmr.symbol;
    val tpe=sybl.typeSignature;
    for{
         bsyble<-tpe.baseClasses
         memb<-NameGetterpeSignature.decls
         if (memb.isTerm)
          // if (tMemb.isGetter)
    }
    yield
    {
         val tMemb=memb.asTerm;
         println(bsyble.name.toString()+"."+memb.name+" : "+memb.annotations.toString());
         tMemb;
    }
    
    fileds.toList;
    Nil;
  }
  def getNameGetters:List[TermSymbol]={
    val getters=this.getGetterFields;
    for {
         getterSybl<-getters
         if (getterSybl.annotations.size>0)
         //anot<-getterSybl.annotations
        // if (anot.tree.tpe==typeOf[NameKind])
       } yield getterSybl 
  }
  def printTypeInfo={
    val crtmr=currentMirror;
    val instmr:InstanceMirror =crtmr.reflect(this);
    val syb=instmr.symbol;
    for (clsSyb:Symbol<-syb.baseClasses){
       println("["+clsSyb.fullName+"]:"); 
      
       println("typeSignature : "+clsSyb.typeSignature);
       println("isTrait: "+clsSyb.asClass.isTrait);
       for( member:Symbol<-clsSyb.typeSignature.decls){
         println(member+{if (member.isTerm) " isStable: "+ member.asTerm.isStable else ""}+
             {if (member.annotations.size>0) " @ "+member.annotations.toString() else ""});
       }
       println("---------------------------------------")
    }
  }
  def getAnnotationMap={
    val crtmr=currentMirror;
    val instmr:InstanceMirror =crtmr.reflect(this);
    val syb=instmr.symbol;
    for {s<-syb.baseClasses;
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
  }
 
}

/**
 * A hack to group XML nodes in one node for output.
 *
 *  @author  lan tian
 *  @version 1.0
 */
trait NameTypeAuthority
{
   def name:String;
   def description:String;
   def managedNameTyps:List[NameType];
}

/**
 * 
 */
trait NameType
{
   def name:String;
   def description:String;
   def authority:NameTypeAuthority;
}
trait Name{
  def name:String;
  def nameType:NameType;
}
trait NameManageService{
  def getNameType(FieldPath:String):NameType;
  def getAuthority(nameType:NameType):NameTypeAuthority;
  def getManagedNameTypes(nameTypeAuthority:NameTypeAuthority):List[NameType];
}
object UndefinedNameType extends NameType{
   def name:String=sys.error("unspported!");
   def description:String=sys.error("unspported!");
   def authority:NameTypeAuthority=UndefinedNameTypeAuthority;
}
 object UndefinedNameTypeAuthority extends  NameTypeAuthority{
   def name:String=sys.error("unspported!");
   def description:String=sys.error("unspported!");
   def managedNameTyps:List[NameType]=sys.error("unspported!");
}
 case class FieldMetaInfo(val ownerPath:String,val field:String,val label:String){
   
 }
 private class NoTypedName (val name:String,val description:String ,val fieldPath:String)extends NameType{
  require(name!=null);
  require(fieldPath!=null);
  def nameType:NameType=UndefinedNameType;
  override def toString=s"[NoTypedName=name: $name ,path: $fieldPath]" ;
}

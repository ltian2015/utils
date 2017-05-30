package ops
package common
import java.time.LocalDateTime;
import ops.common.annotations._
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime._
import scala.reflect.runtime.universe._
import scala.collection.concurrent.TrieMap
import java.time.format.DateTimeFormatterBuilder

/**
 * @author lantian
 */
package object common{
  type NameGenerator=(String,String)=>Name;
  
  /**
   * 这是一个线程安全的映射，用来存储每个SymbolName对应的IdentifiedObject 的元数据信息
   */
  private[this] val idfObjMetaInfoMap = TrieMap.empty[SymbolName, IdentifiedObjectMetaInfo];
  def getTypeMetaInfo[T<:IdentifiedObject](implicit tt:TypeTag[T]):Option[IdentifiedObjectMetaInfo]={
     
      if( !tt.tpe.typeSymbol.isClass) 
         None
       else{
         val clsSmbl:ClassSymbol=tt.tpe.typeSymbol.asClass;
         val sn:SymbolName=SymbolName(clsSmbl.fullName,getLabelValue(clsSmbl));
         
         def createMetaInfo:IdentifiedObjectMetaInfo={
           val nameFields=getNameFields(clsSmbl);
           val timeSensitiveFields=getTimeSensitiveFields(clsSmbl);
           IdentifiedObjectMetaInfo(sn,nameFields,timeSensitiveFields);
         }
         Some(idfObjMetaInfoMap.getOrElseUpdate(sn, createMetaInfo));
       }
  }
  
  private[this] def getLabelValue(smbl:Symbol):String={
    "";
  }
  private[this] def getNameFields(clsSmbl:ClassSymbol):List[SymbolName]={
    Nil;
  }
  private[this] def getTimeSensitiveFields(clsSmbl:ClassSymbol):List[SymbolName]={
    Nil;
  }
  /**
  def getObjMetaInfo(idfObj:IdentifiedObject):IdentifiedObjectMetaInfo={
    val crtmr=currentMirror;
    val instmr:InstanceMirror =crtmr.reflect(idfObj);
    val clsSyb:ClassSymbol=instmr.symbol;
    val clsFullPathName=clsSyb.typeSignature.typeSymbol.fullName;
    println(s"clsFullPathName-$clsFullPathName")
    val typeLabelTag:Option[Annotation]=clsSyb.annotations.find(_.tree.tpe=:=typeTag[Label].tpe);
    println(clsSyb.annotations.head.tree.tpe)
    println(s"typeLabelTag-$typeLabelTag")
    
    val at=idfObj.getClass.getAnnotationsByType(classOf[Label])
    println("*****************************")
    println(idfObj.getClass.getTypeName);
    println(at);
    println(at(0).value());
    println(idfObj.getClass.get);
    println("*****************************")
    val typeLabelValue:String=typeLabelTag match {
      case None =>"";
      case Some(label)=>{
           import scala.tools.reflect.ToolBox
           val toolbox = crtmr.mkToolBox()
           val t:Tree=label.tree
            println(toolbox.untypecheck(label.tree))
            println(t.children.tail.head.children.tail);
            println("value: "+label.asInstanceOf[Label].value());
            println(toolbox.untypecheck(label.tree))
           println(toolbox.eval(toolbox.untypecheck(label.tree)))
           val  typeLabel:Label= toolbox.eval(toolbox.untypecheck(label.tree)).asInstanceOf[Label]
           println(typeLabel);
           ""
      }
    }
    val sn:SymbolName=new SymbolNameInfo(clsFullPathName,typeLabelValue);  
    def loadMetaInfo:IdentifiedObjectMetaInfo={
      new IdfObjectMetaInfo(sn, false,  Nil,Nil);
    }
    this.idfObjMetaInfoMap.getOrElseUpdate(sn, loadMetaInfo);
  } 
  * 
  */
}
trait IdentifiedObject 
{
   @Label("主资源ID")
   def mRID:String;
   
   @NameField
   @Label("名称")
   def name:String;
   
   @NameField
   @Label("别名")
   def aliasName:String;
   
   private[this]  def noTypedNameGenerator(fieldFullPathName:String,nameValue:String):Name=new NoTypeName(fieldFullPathName,nameValue);
   import common._
   def allNames(implicit nameGenerator:NameGenerator=noTypedNameGenerator):List[Name]={
    
     val names:ListBuffer[Name]=ListBuffer();
     for(i<-1 to 5){
       names+=nameGenerator(s"name$i",s"path:$i")
     }
     names.toList; 
     
   }
  
   def printMetaInfo={
    val metaInfo= null;//common.getObjMetaInfo(this);
    println(metaInfo);
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
  /**
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
 */
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
object NoAuthorityNameType extends NameType{
   def name:String="$NoAuthorityName";
   def description:String=sys.error("未定义授权来源的名字!");
   def authority:NameTypeAuthority=UndefinedNameTypeAuthority;
}
 object UndefinedNameTypeAuthority extends  NameTypeAuthority{
   def name:String=sys.error("unspported!");
   def description:String=sys.error("unspported!");
   def managedNameTyps:List[NameType]=sys.error("unspported!");
}
/**
 * @author lant
 */
 sealed trait SymbolName{
   def fullPathName:String;
   def label:String;
   //TODO
   def localName:String=fullPathName;
   //TODO
   def ownerName:String=fullPathName;
   override def toString=s"$fullPathName-$label";
 }
 object SymbolName{
   def apply( fullPathName:String, label:String=""):SymbolName=new SymbolNameInfo(fullPathName,label);
 }
 
 private case class SymbolNameInfo(val fullPathName:String,val label:String="") extends SymbolName{
   require(fullPathName!=null);
    override def equals(other:Any)=
      {
        other match {
          case that:SymbolNameInfo=>(this.fullPathName==that.fullPathName);
          case _ =>false;
        }
      }
   override def hashCode:Int=this.fullPathName.hashCode()*31                           
 }
 
 sealed trait IdentifiedObjectMetaInfo{
  def  symbolName:SymbolName;
  def  nameFields:List[SymbolName];
  def  timeSensitiveFields:List[SymbolName];
  def  isTimeSensitive:Boolean=(timeSensitiveFields!=null)&&(timeSensitiveFields.size>0);
 }
 object IdentifiedObjectMetaInfo{
   def apply(symbolName:SymbolName, nameFields:List[SymbolName],
        timeSensitiveFields:List[SymbolName]):IdentifiedObjectMetaInfo={
         new IdfObjectMetaInfo(symbolName,nameFields,timeSensitiveFields);
    }
 }
 private[common] case class IdfObjectMetaInfo(symbolName:SymbolName, nameFields:List[SymbolName],
        timeSensitiveFields:List[SymbolName]) extends IdentifiedObjectMetaInfo
 
 abstract class BaseName(fieldFullPathName:String,nameValue:String) extends Name{
  
   require(nameValue!=null);
   require(fieldFullPathName!=null);
   val name=nameValue;
 }
 private[common]  class NoTypeName (fieldFullPathName:String,nameValue:String)extends BaseName(fieldFullPathName,nameValue){
  
  def nameType:NameType=NoAuthorityNameType;
  override def toString=s"[NoTypedName=name: $name]" ;
}

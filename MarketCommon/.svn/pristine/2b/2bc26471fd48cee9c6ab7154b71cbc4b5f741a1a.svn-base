package market.common
import java.time.LocalDateTime;
import java.util.Calendar;

trait NameTypeAuthority
{
   def name:String;
   def description:String;
}

object SelfTradeCenterAuthority extends NameTypeAuthority
{
  val name="SelfTradeCenterAuthority";
  val description:String="名字由本交易中权威发布，包括政策要求在交易中心注册的售电公司，售电公司内部组织和各种内部是市场结构和产品等的名称"
}
object OtherTradeCenterAuthority extends NameTypeAuthority
{
  val name="OtherTradeCenterAuthority";
  val description:String="名字由其他交易中心业务权威发布,政策要求跨省售电在一个省的交易中心注册即可被其他交易中心共享"
}
object BusinessRegistrationAuthority
{
  val name="BusinessRegistrationAuthority";
  val description:String="名字由工商管理部门权威发布，包括注册码，注册名（发票名字的权威来源），注册地址，法人代表"
}
object BankAuthority extends NameTypeAuthority
{
  val name="BankSystem";
  val description:String="名字由银行组织权威发布"
}
object LocalGridAuthority extends NameTypeAuthority
{
  val name="LocalGridAuthority";
  val description:String="名字由当地电网公司权威发布,主要是电网资源的名字和代码"
}
trait NameType
{
   def name:String;
   def kind:NameKind.NameKind;
   def description:String;
   def nameManager:NameTypeAuthority;
}
object  NameKind  extends Enumeration
{
   type NameKind=Value;
   /**
    *  LocalName ，名称，对应IdentifiedObject对象的name属性，有且有一个
    *  MainAliasName,主要常用别名，对应IdentifiedObject对象的aliasName属性，有且只能有一个
    *  InvoiceName,发票名称，用于账单生成时使用的名字。
    *  AuxiliaryAliasName，辅助性别名，一个对象允许有多个别名，这些别名有利于在大数据查找时起到识别对象的作用
    */
   val LocalName,FullPathName,InvoiceName,MainAliasName,AuxiliaryAliasName,BRCode,BRName=Value;
} 

trait IdentifiedObject 
{
   def mRID:String;
   def name:String;
   def aliasName:String;
   def names:List[Name];
   /**
   def names(manager:NameTypeAuthority):List[Name]={
      for{
          name<-names;
          if (name.nameType.nameManager==manager) 
      } yield name;
   }
   def names(kind:NameKind):List[Name]={
      for{
          name<-names;
          if (name.nameType.kind==kind) 
      } yield name;
   }**/
}

abstract  class BaseIdentifiedObject(val mRID:String,val names:List[Name]) extends IdentifiedObject
{
  require(names!=null && !names.isEmpty);
  require(mRID!=null);
  /**
  lazy val  name:String={
       val fullNames:List[Name]= for{name <- names
          if (name.nameType.n) 
       }yield name;
       if (fullNames.isEmpty) "" else fullNames(0).name;
   }
   lazy val  aliasName:String={
     val mainAliasNames:List[Name]= for{name <- names
          if (name.nameType == MainAliasName) 
       }yield name;
       if (mainAliasNames.isEmpty) "" else mainAliasNames(0).name;
   };
   def this( mRID:String, name:String,aliasName:String="")={
     this(mRID,Name(name,LocalName)::Name(aliasName,MainAliasName)::Nil);
   }
   * 
   */
}
object Name
{
   def apply( name:String, nameType:NameType):Name=new Name(name, nameType);
}
class  Name (val name:String,val nameType:NameType){
  require(name!=null);
  require(nameType!=null);
}
///////////////////////////////////////////////////////////////////////////////
trait TradeCenter 
{
   def id:String;
   def name:String;
   def description:String;
}
object BaseTradeCenter
{
  def apply( id:String, name:String, description:String=""):TradeCenter=
  {
    new BaseTradeCenter(id,name,description);
  }
}
class BaseTradeCenter(val id:String,val name:String,val description:String="") extends TradeCenter
{
  def toXML=
       <TradeCenter id={id} name={name} description={description} />
}
////////////////////////////////////////////////////////////////////////////////////


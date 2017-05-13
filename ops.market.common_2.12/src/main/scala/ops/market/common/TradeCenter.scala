package ops
package market
package common
import ops.market.common.annocation._
//////////////////////////////////////////////////////////////////////
trait TradeCenter extends IdentifiedObject
{
   def description:String;
   def toXML=
       <TradeCenter id={mRID} name={name} description={description} />
}
object BaseTradeCenter
{
  def apply( id:String, name:String, description:String=""):TradeCenter=
  {
    new BaseTradeCenter(id,name,description);
  }
}
trait Address
{
 // @NameKind(SSNameKind.StreetAddress)
  def addr:String;
}
class BaseTradeCenter(val mRID:String,val name:String,
     val aliasName:String,val description:String="") extends TradeCenter with Address
{
 // @NameKind(SSNameKind.AliasName)
  val myName:String="hello";
 // @NameKind(SSNameKind.AliasName)
  def hisName:String="hellos";
  val addr:String="huangpu road 209";
  
}
class RealTradeCenter( mRID:String, name:String,
      aliasName:String) extends BaseTradeCenter(mRID,name,aliasName)
{
   final val stableName="Lantian"
   var age=1;
   final var hairColor="Red"
   def hello="hello";
}

////////////////////////////////////////////////////////////////////////////////////


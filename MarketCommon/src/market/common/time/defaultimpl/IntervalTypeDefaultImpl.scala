package market.common.time.defaultimpl
import market.common.time.IntervalTypeDecoder
import market.common.TradeCenter;
import market.common.time.AbstractMarketIntervalType
import market.common.time.MktInterval
import market.common.time.MktCalendar
import market.common.time.MarketIntervalUnit
import market.common.time._
import java.time.OffsetDateTime
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object DefaultCalendarFactory{
   private lazy val factory:DefaultMktCalendarFactory={
      new DefaultMktCalendarFactory;
   }
   def apply():MktCalendarFactory=this.factory;
   //for java
   def getInstance()=this.apply();
   
   private final class DefaultMktCalendarFactory extends MktCalendarFactory{
      def getCalendar(tradeCenterId:String):MktCalendar={
        new DefaultMarketCalendar(this,tradeCenterId,DefaultDecoderFactory());
     }
   }
}
    /**
  * 市场日历的缺省实现
  */
 private final class DefaultMarketCalendar(mcFactory:MktCalendarFactory,
      val tradeCenterId:String,val decoderFactory:IntervalDecoderFactory) extends MktCalendar
 {
   require(mcFactory!=null,"构造MarketCalendar 需要指定mcFactory参数 "); 
   require(tradeCenterId!=null,"构造MarketCalendar需要指定所在交易中心ID参数 "); 
   require(decoderFactory!=null,"构造MarketCalendar需要指定解码器工厂参数 "); 
   private lazy val repoImpl:MktCalendarRepository=new RepositoryImpl(this);
   private lazy val tc={ 
     repoImpl.loadTradeCenter;
   }
   require(tc!=null,"指定的交易中心ID参数无效");
   /**
    * 实现了特质所要求的方法。
    */
   def tradeCenter:TradeCenter={this.tc};
   /**
    * 用一个常量的定义方式实现了特质所要求的方法。
    */
   lazy val intervalTypes:List[MarketIntervalType]={
     val itvList=this.repoImpl.loadRegisteredIntervalTypes;
     itvList.sortWith(_ > _);
   }
   doTypesChecking();
   /**
    * 进行市场时段类型检查，主要检查时段的一致性，所有时段不能有相同的跨越时间，
    * 且在时间起点上必须对齐，确保能由大到小的切割。
    */
   private def  doTypesChecking()
   {
     //TODO
   }
   /**
    * 只要市场日历对象所在的交易中心是一个，则认为两个市场对象日历相等。
    */
   override def equals(other:Any)=
      {
        other match {
          case that:MktCalendar=>this.tc.id==that.tradeCenter.id;
          case _ =>false;
        }
      }
     override def hashCode:Int=this.tc.id.hashCode()*47;
     override def toString:String="MktCalendar : "+this.tc.id;
 }
 
final class DefaultIntervalType(id:String, mktCalendar:MktCalendar, 
             unitCount:Int, intervalUnit:MarketIntervalUnit.MarketIntervalUnit, 
              originTime:LocalDateTime,isStop:Boolean, stopTime:LocalDateTime) 
             extends AbstractMarketIntervalType(id,mktCalendar,
          unitCount,intervalUnit,originTime,isStop ,stopTime){
}

object  DefaultDecoderFactory {
   def apply()=new DefaultDecoderFactory;
   final class DefaultDecoderFactory extends IntervalDecoderFactory
   {
     def getDecoder(mktCalendar:MktCalendar,intervalTypeId:String):IntervalTypeDecoder=new DefaultCommonDecoder(mktCalendar,intervalTypeId);
   }
}

private final class  DefaultCommonDecoder(mktCalendar:MktCalendar,val intervalTypeId:String) extends IntervalTypeDecoder
{
    require(mktCalendar!=null);
    private  val itvType=mktCalendar.getIntervalType(intervalTypeId);
    if (itvType==null) throw new Exception("给定时段类型(id="+intervalTypeId+")不存在");
    private val separator:String="@";
    
    def canDecode(code:String):Boolean={
      code.startsWith(itvType.id);
    } 
    def decode(code:String):MktInterval={
       val subStrs:Array[String]=code.split(separator);
       if (subStrs.size!=2) throw new Exception("时段编码格式非法，无法解码");
       val itvId:String=subStrs(0);
       if (itvId!=intervalTypeId)throw new Exception("时段编码格式非法，无法解码");
       val itvStartStr:String=subStrs(1);
       val itvStartTime:LocalDateTime=LocalDateTime.parse(itvStartStr, DateTimeFormatter.ISO_LOCAL_DATE_TIME);
       if (itvStartTime==null) throw new Exception("时段编码格式非法，无法解码");
       this.itvType.getIntervalInclude(itvStartTime);
    }
    def encode(itv:MktInterval):String={
       if (itv.intervalType!=this.itvType) throw new Exception("给时段的类型(id="
              +itv.intervalType.id+")与解码器所设定的时段类型("+
              intervalTypeId+")不相匹配");
       intervalTypeId+this.separator+itv.start.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
    }
}


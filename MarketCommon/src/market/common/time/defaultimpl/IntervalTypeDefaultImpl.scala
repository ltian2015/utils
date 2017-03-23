package market.common.time.defaultimpl
import market.common.time.IntervalTypeDecoder
import market.common.time.AbstractMarketIntervalType
import market.common.time.MktInterval
import market.common.time.MktCalendar
import market.common.time.MarketIntervalUnit
import market.common.time._
import java.time.OffsetDateTime
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/**
 * 定义时段类型解码/编码器工厂对象的行为规范
 */
trait IntervalDecoderFactory
{
   def getDecoder(tradeCenterId:String,intervalTypeId:String):IntervalTypeDecoder;
}

object DefaultCalendarFactory
 {
   val  repositoryImpl:MktCalendarRepository=RepositoryImpl;
   def apply():MktCalendarFactory=new DefaultMktCalendarFactory(repositoryImpl);
 }
 private class DefaultMktCalendarFactory(val mktCalendarRepo:MktCalendarRepository) extends MktCalendarFactory{
   
   private val calendarMap:Map[String,MktCalendar]=Map.empty;
   def getCalendar(tradeCenterId:String):MktCalendar={
      if( this.calendarMap.contains(tradeCenterId))
         calendarMap(tradeCenterId);
     else {
       val calendar=new DefaultMarketCalendar(mktCalendarRepo,tradeCenterId);
       calendarMap + (tradeCenterId->calendar);
       calendar;
     } 
   }
 }
class BaseMarketIntervalType(id:String, mktCalendar:MktCalendar, 
             unitCount:Int, intervalUnit:MarketIntervalUnit.MarketIntervalUnit, 
             decoder:IntervalTypeDecoder, startTime:LocalDateTime,
             isStop:Boolean, stopTime:LocalDateTime) 
             extends AbstractMarketIntervalType(id,mktCalendar,
          unitCount,intervalUnit,decoder,startTime,isStop ,stopTime)
{
  
}

final class ThreeYear(mktCalendar:MktCalendar,startTime:LocalDateTime,isStop:Boolean, stopTime:LocalDateTime) 
      extends BaseMarketIntervalType("3Y", mktCalendar,
            3,MarketIntervalUnit.YEAR,ThreeYearDecoder,startTime,isStop,stopTime)
{
  
}

final class TwoYear(mktCalendar:MktCalendar,startTime:LocalDateTime,isStop:Boolean, stopTime:LocalDateTime) 
      extends BaseMarketIntervalType("2Y", mktCalendar,
            2,MarketIntervalUnit.YEAR,TwoYearDecoder,startTime,isStop,stopTime)
{
  
}
final class OneYear(mktCalendar:MktCalendar,startTime:LocalDateTime,isStop:Boolean, stopTime:LocalDateTime) 
      extends BaseMarketIntervalType("1Y", mktCalendar,
            1,MarketIntervalUnit.YEAR,OneYearDecoder,startTime,isStop,stopTime)
{

}
final class OneMonth(mktCalendar:MktCalendar,startTime:LocalDateTime,isStop:Boolean, stopTime:LocalDateTime) 
      extends BaseMarketIntervalType("1M", mktCalendar,
            1,MarketIntervalUnit.MONTH,OneMonthDecoder,startTime,isStop,stopTime)
{

}
final class OneDay(mktCalendar:MktCalendar,startTime:LocalDateTime,isStop:Boolean, stopTime:LocalDateTime) 
      extends BaseMarketIntervalType("1D", mktCalendar,
            1,MarketIntervalUnit.DAY,OneDayDecoder,startTime,isStop,stopTime)
{

}
final class OneHour(mktCalendar:MktCalendar,startTime:LocalDateTime,isStop:Boolean, stopTime:LocalDateTime) 
      extends BaseMarketIntervalType("1H", mktCalendar,
            1,MarketIntervalUnit.HOUR,OneHourDecoder,startTime,isStop,stopTime)
{

}
final class OneQuarter(mktCalendar:MktCalendar,startTime:LocalDateTime,isStop:Boolean, stopTime:LocalDateTime) 
      extends BaseMarketIntervalType("1Q", mktCalendar,
            15,MarketIntervalUnit.MINUTE,OneQuarterDecoder,startTime,isStop,stopTime)
{

}
final class FiveMinute(mktCalendar:MktCalendar,startTime:LocalDateTime,isStop:Boolean, stopTime:LocalDateTime) 
      extends BaseMarketIntervalType("5m", mktCalendar,5,
          MarketIntervalUnit.MINUTE,FiveMinuteDecoder,startTime,isStop,stopTime)
{

}

object  DefaultDecoderFactory extends IntervalDecoderFactory
{
   def getDecoder(tradeCenterId:String,intervalTypeId:String):IntervalTypeDecoder={
       new DefaultCommonDecoder(tradeCenterId,intervalTypeId);
   }
}
final class  DefaultCommonDecoder(val tradeCenterId:String,val intervalTypeId:String) extends IntervalTypeDecoder
{
    private  val mc=DefaultCalendarFactory().getCalendar(tradeCenterId);
    if (mc==null) throw new Exception("给定ID："+tradeCenterId+"的交易中心不存在");
    private  val itvType=mc.getIntervalType(intervalTypeId);
    if (itvType==null) throw new Exception("给定ID："+intervalTypeId+"的时段类型不存在");
    private val separator:String=":";
    
    def canDecode(itvId:String):Boolean={
      itvId.startsWith(itvType.id);
    } 
    def decode(itvId:String):MktInterval={
       val subStrs:Array[String]=itvType.id.split(separator);
       if (subStrs.size!=2) throw new Exception("给定时段编码格式非法，无法解码");
       val itvId:String=subStrs(1);
       if (itvId!=intervalTypeId)throw new Exception("给定时段编码格式非法，无法解码");
       val itvStartStr:String=subStrs(2);
       val itvStartTime:LocalDateTime=LocalDateTime.parse(itvStartStr, DateTimeFormatter.ISO_LOCAL_DATE_TIME);
       if (itvStartTime==null) throw new Exception("给定时段编码格式非法，无法解码");
       this.itvType.getIntervalInclude(itvStartTime);
    }
    def encode(itv:MktInterval):String={
       if (itv.intervalType!=this.itvType) throw new Exception("给定的时段对象类型(id="
              +itv.intervalType.id+")与本解码器所能解码的时段类型("+
              intervalTypeId+")+不匹配，无法解码");
       intervalTypeId+this.separator+itv.start.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
    }
}


package market.common.time.defaultimpl
import market.common.time.IntervalTypeDecoder
import market.common.time.AbstractMarketIntervalType
import market.common.time.MktInterval
import market.common.time.MktCalendar
import market.common.time.MarketIntervalUnit
import market.common.time._
import java.time.OffsetDateTime
import java.time.LocalDateTime
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
   def getDecoder(intervalTypeId:String):IntervalTypeDecoder={
     intervalTypeId match {
       case "3Y" =>ThreeYearDecoder;
       case "2Y" =>TwoYearDecoder;
       case "1Y" =>OneYearDecoder;
       case "1M" =>OneMonthDecoder;
       case "1D" =>OneDayDecoder;
       case "1H" =>OneHourDecoder;
       case "1Q" =>OneQuarterDecoder;
       case "5m" =>FiveMinuteDecoder;
       case _ =>null;
     }
   }
}
object ThreeYearDecoder extends IntervalTypeDecoder
{
    def canDecode(itvId:String):Boolean=itvId.startsWith("3Y:");
    def decode(itvId:String):MktInterval={
      //TODO
      null;
    }
    def encode(itv:MktInterval):String={
      //TODOs
      "";
    }
}
object TwoYearDecoder extends IntervalTypeDecoder
{
    def canDecode(itvId:String):Boolean=itvId.startsWith("2Y:");
    def decode(itvId:String):MktInterval={
      //TODO
      null;
    }
    def encode(itv:MktInterval):String={
      //TODOs
      "";
    }
}
object OneYearDecoder extends IntervalTypeDecoder
{
    def canDecode(itvId:String):Boolean=itvId.startsWith("1Y:");
    def decode(itvId:String):MktInterval={
      //TODO
      null;
    }
    def encode(itv:MktInterval):String={
      //TODOs
      "";
    }
}
object OneMonthDecoder extends IntervalTypeDecoder
{
    def canDecode(itvId:String):Boolean=itvId.startsWith("1M:");
    def decode(itvId:String):MktInterval={
      //TODO
      null;
    }
    def encode(itv:MktInterval):String={
      //TODOs
      "";
    }
}
object OneDayDecoder extends IntervalTypeDecoder
{
    def canDecode(itvId:String):Boolean=itvId.startsWith("1D:");
    def decode(itvId:String):MktInterval={
      //TODO
      null;
    }
    def encode(itv:MktInterval):String={
      //TODOs
      "";
    }
}
object OneHourDecoder extends IntervalTypeDecoder
{
    def canDecode(itvId:String):Boolean=itvId.startsWith("1H:");
    def decode(itvId:String):MktInterval={
      //TODO
      null;
    }
    def encode(itv:MktInterval):String={
      //TODOs
      "";
    }
}
object OneQuarterDecoder extends IntervalTypeDecoder
{
    def canDecode(itvId:String):Boolean=itvId.startsWith("1Q:");
    def decode(itvId:String):MktInterval={
      //TODO
      null;
    }
    def encode(itv:MktInterval):String={
      //TODOs
      "";
    }
}
object FiveMinuteDecoder extends IntervalTypeDecoder
{
   def canDecode(itvId:String):Boolean=itvId.startsWith("5m:");
    def decode(itvId:String):MktInterval={
      //TODO
      null;
    }
    def encode(itv:MktInterval):String={
      //TODOs
      "";
    }
}


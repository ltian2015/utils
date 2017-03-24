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

object DefaultCalendarFactory
 {
   val  repositoryImpl:MktCalendarRepository=RepositoryImpl;
   def apply():MktCalendarFactory=new DefaultMktCalendarFactory(repositoryImpl);
   //for java
   def getInstance()=this.apply();
 }

 private final class DefaultMktCalendarFactory(val mktCalendarRepo:MktCalendarRepository) extends MktCalendarFactory{
   
   private val calendarMap:Map[String,MktCalendar]=Map.empty;
   def getCalendar(tradeCenterId:String):MktCalendar={
      if( this.calendarMap.contains(tradeCenterId))
         calendarMap(tradeCenterId);
     else {
       val calendar=new DefaultMarketCalendar(mktCalendarRepo,tradeCenterId,DefaultDecoderFactory(tradeCenterId));
       calendarMap + (tradeCenterId->calendar);
       calendar;
     } 
   }
 }
 final class DefaultIntervalType(id:String, mktCalendar:MktCalendar, 
             unitCount:Int, intervalUnit:MarketIntervalUnit.MarketIntervalUnit, 
              startTime:LocalDateTime,isStop:Boolean, stopTime:LocalDateTime) 
             extends AbstractMarketIntervalType(id,mktCalendar,
          unitCount,intervalUnit,startTime,isStop ,stopTime)
{
    
}

object  DefaultDecoderFactory 
{
   def apply(tradeCenterId:String)=new DefaultDecoderFactory(tradeCenterId);
   final class DefaultDecoderFactory(val tradeCenterId:String) extends IntervalDecoderFactory
   {
     def getDecoder(intervalTypeId:String):IntervalTypeDecoder=new DefaultCommonDecoder(this.tradeCenterId,intervalTypeId);
   }
}

private final class  DefaultCommonDecoder(val tradeCenterId:String,val intervalTypeId:String) extends IntervalTypeDecoder
{
    private  val mc=DefaultCalendarFactory().getCalendar(tradeCenterId);
    if (mc==null) throw new Exception("给定的交易中心(id="+tradeCenterId+")不存在");
    private  val itvType=mc.getIntervalType(intervalTypeId);
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


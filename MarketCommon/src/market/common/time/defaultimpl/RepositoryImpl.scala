package market.common.time.defaultimpl
import market.common.time._
import market.common._
import scala.collection.mutable.ListBuffer;
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import market.common.time.defaultimpl._
object RepositoryImpl extends MktCalendarRepository{
  val dir=".\\resource\\";
  val tcConfigFile=dir+"TradeCenterConfig.xml"
  val tcIntConfigFilePrex=dir+"MarketIntervalTypeConfig."
  def  loadRegisteredIntervalTypes(tradeCenterId:String):List[MarketIntervalType]=
  {
    val rootNode=scala.xml.XML.load(tcIntConfigFilePrex+tradeCenterId+".xml");
    val listBuffer=new ListBuffer[MarketIntervalType]();
    val calendar=DefaultCalendarFactory().getCalendar(tradeCenterId);
    for(node <- (rootNode \ "MarketIntervalType")){
            val startTimeText=(node \ "@startTime").text;
            val startTime:LocalDateTime=LocalDateTime.parse(startTimeText,DateTimeFormatter.ISO_LOCAL_DATE_TIME);
            val isStop=(node \ "@isStop").text.toBoolean;
            val stopTimeText=(node \ "@stopTime").text;
            val stopTime:LocalDateTime=if (stopTimeText=="" || stopTimeText=="-") null 
                 else LocalDateTime.parse(stopTimeText,DateTimeFormatter.ISO_LOCAL_DATE_TIME);  
            val itvType:MarketIntervalType=(node \ "@id").text match {
                 case  "3Y" =>new ThreeYear(calendar,startTime,isStop,stopTime);
                 case  "2Y" =>new TwoYear(calendar,startTime,isStop,stopTime);
                 case  "1Y" =>new OneYear(calendar,startTime,isStop,stopTime);
                 case  "1M" =>new OneMonth(calendar,startTime,isStop,stopTime);
                 case  "1D" =>new OneDay(calendar,startTime,isStop,stopTime);
                 case  "1Q" =>new OneQuarter(calendar,startTime,isStop,stopTime);
                 case  "5m" =>new FiveMinute(calendar,startTime,isStop,stopTime);
                 case _ =>{ val unitCount= (node \ "@unitCount").text.toInt;
                     val intervalUnit=MarketIntervalUnit.from((node \ "@intervalUnit").text);
                     val typeId=(node \ "@id").text;
                     val decoder=DefaultDecoderFactory.getDecoder(typeId);
                     new BaseMarketIntervalType(typeId,
                         calendar, unitCount, intervalUnit, decoder,startTime,isStop,stopTime);
                 }
            }
            listBuffer += itvType;
    }
    listBuffer.toList;
  }
  def  saveNewIntervalType(itvType:MarketIntervalType):Unit={
     //TODO
  }
  def  updateIntervalType(itvType:MarketIntervalType):Unit={
     //TODO
  };
  def loadTradeCenter(tradeCenterId:String):TradeCenter={
    val rootNode=scala.xml.XML.load(tcConfigFile);
    val listBuffer=new ListBuffer[TradeCenter]();
    for(node <- (rootNode \ "TradeCenter")){
       if ((node \@"id")==tradeCenterId)
       {
        val tc:TradeCenter=BaseTradeCenter( (node \ "@id").text ,
                              (node \ "@name").text,
                              (node \ "@description").text );
        listBuffer += tc;
       }
    }
    if (listBuffer.isEmpty) 
       throw new Exception("")
    else if (listBuffer.size>1)
        throw new Exception("");
    else listBuffer(0);
  };
  
}
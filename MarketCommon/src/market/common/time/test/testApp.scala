package market.common.time.test
import java.time.MonthDay;
import java.time.LocalDate;
import java.time.OffsetTime;
import java.util.Calendar;
import java.time.ZoneOffset;
import java.time.ZoneId;
import java.time.LocalDateTime;
import java.time.Instant;
import java.time.OffsetDateTime;
import scala.collection.mutable.ListBuffer
import scala.xml.Elem
import market.common._;
import market.common.time._;
import market.common.time.defaultimpl._
import java.time.Period
import java.time.temporal.ChronoUnit ;

object Test extends App{
  val dir=".\\resource\\";
  val tcConfigfileName=dir+"TradeCenterConfig.xml";
  val tcIntConfigFilePrex=dir+"MarketIntervalTypeConfig."
  private def creatTcXML={
    val kunmingTc:BaseTradeCenter=new BaseTradeCenter("kunming","昆明电力交易中心","测试租户");
    val rootNode=
      <TradeCenters>
    {kunmingTc.toXML};
</TradeCenters>;
    scala.xml.XML.save(tcConfigfileName, rootNode, "UTF-8", true, null);
  }
  private def readTcConfig:List[TradeCenter]={
    val rootNode=scala.xml.XML.loadFile(tcConfigfileName);
    val listbuffer:ListBuffer[TradeCenter]=new ListBuffer();
    for{node <- (rootNode \ "TradeCenter")}{
        val tc:TradeCenter=BaseTradeCenter( (node \ "@id").text ,
                              (node \ "@name").text,
                              (node \ "@description").text );
        listbuffer += tc;
    }
        listbuffer.toList;
  }
  
  private def creatIntervalConfig(tcId:String)=
  {
     val mc: MktCalendar=DefaultCalendarFactory().getCalendar("kunming")
     val fileName=tcIntConfigFilePrex+mc.tradeCenter.id+".xml"
     val startTime=LocalDateTime.of(2017, 1, 1, 0, 0,0, 0);
     val itvType1=new ThreeYear(mc,startTime,false,null);
   //  val itvType2=new TwoYear(mc,startTime,false,null);
     val itvType3=new OneYear(mc,startTime,false,null);
     val itvType4=new OneMonth(mc,startTime,false,null);
     val itvType5=new OneDay(mc,startTime,false,null);
     val itvType6=new OneHour(mc,startTime,false,null);
     val itvType7=new OneQuarter(mc,startTime,false,null);
     val itvType8=new FiveMinute(mc,startTime,false,null);
     val rootNode:Elem=
      <MktIntervalTypes>
        {itvType1.toXML}
        {itvType3.toXML}
        {itvType4.toXML}
        {itvType5.toXML}
        {itvType6.toXML}
        {itvType7.toXML}
        {itvType8.toXML}
</MktIntervalTypes>;
 scala.xml.XML.save(fileName, rootNode, "UTF-8", true, null);
}

  // creatIntervalConfig("kunming");
     val mc: MktCalendar=DefaultCalendarFactory().getCalendar("kunming")
     val itvType3Y:MarketIntervalType=mc.getIntervalType(PredefIntverType.THREE_YEAR);  
     val itvType1Y=mc.getIntervalType(PredefIntverType.ONE_YEAR);
     val itvType1M=mc.getIntervalType((1,MarketIntervalUnit.MONTH));
     val itvType1D=mc.getIntervalType("1D");
     val itvType1H=mc.getIntervalType("1H");
     val itvType1Q=mc.getIntervalType("1Q");
     val itvType5m=mc.getIntervalType("5m");
     val itv=itvType3Y.getStartingInterval;
     val startInclusive=LocalDateTime.of(2017, 1, 1, 0, 0);
     val endExclusive=LocalDateTime.of(2018, 1, 4, 0, 1);
     val itv1=mc.getIntervalAt(startInclusive, endExclusive)
     println(itv1);
     
     println("---------------------------------------------------")
     //val itvList=itvType1D.getIntervalsCross(startInclusive, endExclusive);
     val itvList=mc.getIntervalsTo(PredefIntverType.ONE_DAY,  endExclusive);
     for (itv<-itvList) println(itv.toString()+":"+itv.isBefor(endExclusive));
    
              

}


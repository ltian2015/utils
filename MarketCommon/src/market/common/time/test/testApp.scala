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
     val itvType3Y=new DefaultIntervalType("3Y",mc,3,MarketIntervalUnit.YEAR,startTime,false,null);
     //val itvType2Y=new DefaultIntervalType("2Y",mc,2,MarketIntervalUnit.MONTH,startTime,false,null);
     val itvType1Y=new DefaultIntervalType("1Y",mc,1,MarketIntervalUnit.YEAR,startTime,false,null);
     val itvType1M=new DefaultIntervalType("1M",mc,1,MarketIntervalUnit.MONTH,startTime,false,null);
     val itvType1D=new DefaultIntervalType("1D",mc,1,MarketIntervalUnit.DAY,startTime,false,null);
     val itvType1H=new DefaultIntervalType("1H",mc,1,MarketIntervalUnit.HOUR,startTime,false,null);
     val itvType1Q=new DefaultIntervalType("1Q",mc,15,MarketIntervalUnit.MINUTE,startTime,false,null);
     val itvType5m=new DefaultIntervalType("5m",mc,5,MarketIntervalUnit.MINUTE,startTime,false,null);
     val rootNode:Elem=
      <MktIntervalTypes>
        {itvType3Y.toXML}
        {itvType1Y.toXML}
        {itvType1M.toXML}
        {itvType1D.toXML}
        {itvType1H.toXML}
        {itvType1Q.toXML}
        {itvType5m.toXML}
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
     val itv=itvType3Y.getOrginInterval;
     val startInclusive=LocalDateTime.of(2017, 1, 1, 0, 0);
     val endExclusive=LocalDateTime.of(2017, 1, 2, 0, 0);
     val itv1=mc.getIntervalAt(startInclusive, endExclusive)
     println(itv1);
     
     println("---------------------------------------------------")
     //val itvList=itvType1D.getIntervalsCross(startInclusive, endExclusive);
     val itvList=mc.getIntervalsCross(itvType1Q, startInclusive, endExclusive)
     println(itv1.getSubIntervalCounts(itvType1Q));
     for (itv<-itvList) {
       println(itv.toString()+":"+itv.isBefor(endExclusive)+" : "+itv.encode);
     }
     val itv2=mc.decode("5m@2017-01-01T23:55:00");
      println("---------------------------------------------------")   
     println(itv2.intervalType)
     val itv3=itv2.prior(true);
      println(itv2>=itv3);
     
     println("hello svn");
     println("hello git");
     println("hello exit svn to git")
}


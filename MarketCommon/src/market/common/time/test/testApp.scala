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
     for (itvType <-mc.intervalTypes)
     {
        println(itvType.toString()) 
       
     }
      println(mc.getMaxIntervalType.toString());
      println(mc.getMinIntervalType.toString())
     val itvType3Y:MarketIntervalType=mc.getIntervalType(PredefIntverType.THREE_YEAR);  
     //val itvType2Y=mc.getIntervalType(PredefIntverType.TWO_YEAR);
     val itvType1Y=mc.getIntervalType(PredefIntverType.ONE_YEAR);
     println(itvType1Y.getMktIntervalInclude(LocalDateTime.now()));
     val itvType1M=mc.getIntervalType((1,MarketIntervalUnit.MONTH));
    // val itvType1M=mc.getIntervalType(1,MarketIntervalUnit.MONTH);
      println(itvType1M.getMktIntervalInclude(LocalDateTime.now()));
     val itvType1D=mc.getIntervalType("1D");
      println(itvType1D.getMktIntervalInclude(LocalDateTime.now()));
      val itvType1H=mc.getIntervalType("1H");
       println(itvType1H.getMktIntervalInclude(LocalDateTime.now()));
     val itvType1Q=mc.getIntervalType("1Q");
      println(itvType1Q.getMktIntervalInclude(LocalDateTime.now()));
     val itvType5m=mc.getIntervalType("5m");
     println(itvType5m.getMktIntervalInclude(LocalDateTime.now()));
     val itv3Y=itvType3Y.getMktIntervalInclude(LocalDateTime.now());
     //println(itv3Y);
     val itv1M=itv3Y.getSubIntervals(itvType1M)(0);
     val itv1D=mc.getIntervalType("1D").getMktIntervalInclude(LocalDateTime.now());
    // println(itv1D);
    // for(itv5m<-itv1D.getLowerIntervals(itvType5m)) println(itv5m);
     val list=itv1D.getSubIntervals(itvType1Q)
     val itvFirst=list(0);
     //println(itvFirst);
    // println(itvFirst.next);
   //  println(itvFirst.prior);
     println("////////////////////////////////////////////////////////////////////////////");
     val itvList=itv1D.getSubIntervals(itvType1Q);
     for(itv<-itvList ) println(itv);
     println("////////////////////////////////////////////////////////////////////////////");
     println(itvList.size);
     val itv=itvList.last;
     println(itv);
     println(itv.toDuration);
     println(itv.toPeriod.getDays);
     println(itv3Y);
     println(itv3Y.toPeriod.getDays);
     println(itv3Y.start.toLocalDate());
     println(itv3Y.end.toLocalDate());
     val p=Period.between(itv3Y.start.toLocalDate(), itv3Y.end.toLocalDate());
     println(p);
     println(itv.toDuration);
     println(itv.toDuration.getSeconds);
     println(p.getYears);
     println(p.get(ChronoUnit.DAYS));
     println(p.get(ChronoUnit.MONTHS));
     println(itv.toTuple._1)
     println(itv.toTuple._2)
    
   

}


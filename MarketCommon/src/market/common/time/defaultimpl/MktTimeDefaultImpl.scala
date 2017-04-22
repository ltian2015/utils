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
import market.common.TradeCenter
import market.common.BaseTradeCenter
import scala.collection.mutable.ListBuffer;
/**
 * 缺省的市场日历工厂实现
 */
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
   private lazy val repoImpl:MktCalendarRepository=new DefaultRepositoryImpl(this);
   private lazy val tc=repoImpl.loadTradeCenter;
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
 /**
  * MktCalendarRepository接口的实现，真正提供对市场日历相关持久化的访问。
  */
 class DefaultRepositoryImpl(calendar:MktCalendar) extends BaseMktCalendarRepository(calendar){
  val tcConfigUrl:java.net.URL=this.getClass.getResource("/resource/TradeCenterConfig.xml");
  def  loadRegisteredIntervalTypes:List[MarketIntervalType]={
    val tradeCenterId=this.calendar.tradeCenterId;
    val itvTypeConfigUrl=this.getClass.getResource("/resource/MarketIntervalTypeConfig."+tradeCenterId+".xml");
    val rootNode=scala.xml.XML.load(itvTypeConfigUrl);
    val listBuffer=ListBuffer[MarketIntervalType]();
    for(node <- (rootNode \ "MarketIntervalType")){
            val originTimeText=(node \ "@originTime").text;
            val startTime:LocalDateTime=LocalDateTime.parse(originTimeText,DateTimeFormatter.ISO_LOCAL_DATE_TIME);
            val isStop=(node \ "@isStop").text.toBoolean;
            val stopTimeText=(node \ "@stopTime").text;
            val stopTime:LocalDateTime=if (stopTimeText=="" || stopTimeText=="-") null 
                 else LocalDateTime.parse(stopTimeText,DateTimeFormatter.ISO_LOCAL_DATE_TIME);  
            val typeId=(node \ "@id").text;
            val intervalUnit=MarketIntervalUnit.from((node \ "@intervalUnit").text);
            val unitCount= (node \ "@unitCount").text.toInt;
            val itvType:MarketIntervalType=new DefaultIntervalType(typeId,
                         calendar, unitCount, intervalUnit,startTime,isStop,stopTime);
            listBuffer += itvType;
    }
    listBuffer.toList;
  }
  def  saveNewIntervalType(itvType:MarketIntervalType):Unit={
     //TODO
  }
  def  updateIntervalType(itvType:MarketIntervalType):Unit={
     //TODO
  }
  
  def loadTradeCenter:TradeCenter={
    val tradeCenterId:String=this.calendar.tradeCenterId;
    val rootNode=scala.xml.XML.load(tcConfigUrl);
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
       sys.error("在数据中心配置XML文件中加载不到Id为"+tradeCenterId+"的交易中心定义")
    else if (listBuffer.size>1)
        sys.error("在数据中心配置XML文件中加载了多个Id为"+tradeCenterId+"的交易中心定义");
    else listBuffer(0);
  }
}
/**
 * 缺省的时段类型实现类。
 */
class DefaultIntervalType(id:String, mktCalendar:MktCalendar, 
             unitCount:Int, intervalUnit:MarketIntervalUnit.MarketIntervalUnit, 
              originTime:LocalDateTime,isStop:Boolean, stopTime:LocalDateTime) 
             extends AbstractMarketIntervalType(id,mktCalendar,
          unitCount,intervalUnit,originTime,isStop ,stopTime){
}
/**
 * 缺省的解码器工厂实现类
 */
object  DefaultDecoderFactory {
   def apply()=new DefaultDecoderFactory;
   final class DefaultDecoderFactory extends IntervalDecoderFactory
   {
     def getDecoder(mktCalendar:MktCalendar,intervalTypeId:String):IntervalTypeDecoder=new DefaultCommonDecoder(mktCalendar,intervalTypeId);
   }
}
/**
 * 缺省的通用市场时段解码/编码器的实现。本实现中，合法的时段编码格式为：{时段类型ID}@{时段开始时间的ISO_LOCAL_DATE_TIME格式}
 */
private final class  DefaultCommonDecoder(mktCalendar:MktCalendar,val intervalTypeId:String) extends IntervalTypeDecoder
{
    require(mktCalendar!=null);
    private  val itvType=mktCalendar.getIntervalType(intervalTypeId);
    if (itvType==null) sys.error("给定时段类型(id="+intervalTypeId+")不存在");
    private val separator:String="@";
    
    def canDecode(code:String):Boolean={
      code.startsWith(itvType.id);
    } 
    
    def decode(code:String):MktInterval={
      //TODO 
      /**思考如果无法解码是抛出异常还是给出None ?
                后者可能会更改函数返回类型为Option[MktInterval],这样Java程序调用会不会不习惯呢？
               是为Java程序员另外封装一个API，返回Java8的Optional<MktInterval>，还是就提供此API
              让Java程序员使用Scala 的Option？
      * 
      */
       val subStrs:Array[String]=code.split(separator);
       val itvStartStr:String=subStrs match{
         case Array(this.intervalTypeId,timeStr)=>timeStr
         case _ =>sys.error("时段编码格式非法，无法解码");
       }
       val itvStartTime:LocalDateTime=LocalDateTime.parse(itvStartStr, DateTimeFormatter.ISO_LOCAL_DATE_TIME);
       this.itvType.getIntervalInclude(itvStartTime);
    }
    
    def encode(itv:MktInterval):String={
       if (itv.intervalType!=this.itvType) sys.error("给时段的类型(id="
              +itv.intervalType.id+")与解码器所设定的时段类型("+
              intervalTypeId+")不相匹配");
       intervalTypeId+this.separator+itv.start.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
    }
}


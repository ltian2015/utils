package ops
package market
package common
package time
import java.time.LocalDateTime
import scala.collection.JavaConverters._
import java.time.MonthDay
import java.time.ZoneOffset
import java.lang.Comparable;
import scala.Ordered;
import java.time.temporal.ChronoUnit;
import java.time.format.DateTimeFormatter
import ops.market.common.TradeCenter
import scala.List;
import scala.collection.mutable.ListBuffer;
import java.time.Duration
import java.time.Period
import scala.language.implicitConversions;
/**
 * 
 * 定义了市场时间单位的枚举类型
 */
object MarketIntervalUnit extends Enumeration 
{
  type MarketIntervalUnit=Value;
   val YEAR,MONTH,DAY,HOUR,MINUTE,SECOND=Value;
   def from(value:String):MarketIntervalUnit= value match { 
     case "YEAR" =>MarketIntervalUnit.YEAR
     case "MONTH" =>MarketIntervalUnit.MONTH
     case "DAY" =>MarketIntervalUnit.DAY
     case "HOUR" =>MarketIntervalUnit.HOUR
     case "MINUTE" =>MarketIntervalUnit.MINUTE
     case "SECOND" =>MarketIntervalUnit.SECOND
   }  
   def toChronoUnit(value:Value):ChronoUnit=value match { 
             case MarketIntervalUnit.YEAR =>ChronoUnit.YEARS;
             case MarketIntervalUnit.MONTH=>ChronoUnit.MONTHS;
             case MarketIntervalUnit.DAY=>ChronoUnit.DAYS;
             case MarketIntervalUnit.HOUR=>ChronoUnit.HOURS;
             case MarketIntervalUnit.MINUTE=>ChronoUnit.MINUTES;
             case MarketIntervalUnit.SECOND=>ChronoUnit.SECONDS;
   };
   implicit def marketIntervalUnitWrapper(chronoUnit:ChronoUnit):MarketIntervalUnit.MarketIntervalUnit={
        chronoUnit match { 
               case ChronoUnit.YEARS =>MarketIntervalUnit.YEAR
               case ChronoUnit.MONTHS=>MarketIntervalUnit.MONTH
               case ChronoUnit.DAYS=>MarketIntervalUnit.DAY
               case ChronoUnit.HOURS=>MarketIntervalUnit.HOUR;
               case ChronoUnit.MINUTES=>MarketIntervalUnit.MINUTE;
               case ChronoUnit.SECONDS=>MarketIntervalUnit.SECOND;
               case _=>sys.error("给定的ChronoUnit没有相对应的MarketIntervalUnit");
        }
    }
}
/**
 * 定义市场预定义的时段类型的枚举类型。
 */
object PredefIntverType extends Enumeration 
{
   type PredefIntverType=Value;
   val THREE_YEAR,TWO_YEAR,ONE_YEAR,ONE_MONTH,ONE_DAY,ONE_HOUR,ONE_QUARTER,FIVE_MINUTE=Value;
   def toTuple(predefType:PredefIntverType.PredefIntverType)={ predefType match{
         case THREE_YEAR=>(3,MarketIntervalUnit.YEAR);
         case TWO_YEAR=>(2,MarketIntervalUnit.YEAR);
         case ONE_YEAR=>(1,MarketIntervalUnit.YEAR);
         case ONE_MONTH=>(1,MarketIntervalUnit.MONTH);
         case ONE_DAY=>(1,MarketIntervalUnit.DAY);
         case ONE_HOUR=>(1,MarketIntervalUnit.HOUR);
         case ONE_QUARTER=>(15,MarketIntervalUnit.MINUTE);
         case FIVE_MINUTE=>(5,MarketIntervalUnit.MINUTE);
       }
   }
}
///////////////////////以下是关于市场日历的类定义/////////////////////////////////////////////////////
/**
 * 定义了市场日历对象工厂的行为规范
 */
 trait MktCalendarFactory
 {
   /**
    * 构建市场日历的工厂可以根据交易中心的ID创建市场日历。
    */
   def getCalendar(tradeCenterId:String):MktCalendar;
 }
 
/**
 * 定义了市场日历对象的行为规范
 */
 trait MktCalendar
{
   implicit def mktIntervalWrapper(itv:TimeInterval):MktInterval={
     this.getIntervalAt(itv.start, itv.end);
   }
   //-------------------以下方法必须由实现类实现-----------------------------------------
  
    def  tradeCenterId:String;

   /**
    * 市场日历所在的交易中心
    */
   
   def tradeCenter:TradeCenter;
   /**
    * 市场日历所在交易中心定义的所有时段类型列表
    */
   
   def intervalTypes:List[MarketIntervalType];
   //for java
   def getIntervalTypeList:java.util.List[MarketIntervalType]=this.intervalTypes.asJava;
   def decoderFactory:IntervalDecoderFactory
   //-------------------------------------------------------------- 
  /**
   * 按照步长和市场时间单位获得市场时段类型
   */
   def getIntervalType(unitCount:Int,
       intervalUnit:MarketIntervalUnit.MarketIntervalUnit):MarketIntervalType={
       val listBuffer:ListBuffer[MarketIntervalType]=new ListBuffer() ;
       for(itvType <- intervalTypes){
           if(( itvType.unitCount==unitCount)&&(itvType.intervalUnit==intervalUnit))
              listBuffer += itvType;
       }
       if (listBuffer.size>1) sys.error("严重问题：找到了多个相匹配的时段类型");
       if (listBuffer.size==0) sys.error("严重问题：找不到相匹配的时段类型");
       listBuffer(0);
   }
   /**
    * 根据预定义的枚举型市场时段类型或市场时段类型，推荐用此API。
    */
   def getIntervalType(predefType:PredefIntverType.PredefIntverType):MarketIntervalType={
     this.getIntervalType(PredefIntverType.toTuple(predefType));
   }
   /**
    * 按照步长和市场时间单位组成的二元组获得市场时段类型
    */
   def getIntervalType(tuple:Tuple2[Int,MarketIntervalUnit.MarketIntervalUnit]):MarketIntervalType={
      this.getIntervalType(tuple._1,tuple._2);
   }
   /**
    * 按照市场时段的ID获取市场时段类型，该API通常用在其他在持久化中引用市场时段ID的其他对象访问市场时段类型。
    */
   def getIntervalType(typeId:String):MarketIntervalType={
      val listBuffer:ListBuffer[MarketIntervalType]=new ListBuffer() ;
     for(itvType <- intervalTypes){
         if( itvType.id==typeId) listBuffer += itvType;
     }
     if (listBuffer.size>1) sys.error("严重问题：可以找到多个相匹配的时段类型");
     if(listBuffer.size==0) sys.error("严重问题：找不到相匹配的时段类型");
     listBuffer(0);
   }
   /**
    * 获取最长的市场时段类型
    */
   def getMaxIntervalType:MarketIntervalType={
     if ((this.intervalTypes!=Nil)&&(!this.intervalTypes.isEmpty))
         this.intervalTypes(0);
     else null;
   }
   /**
    * 获取最短的市场时段类型
    */
   def getMinIntervalType:MarketIntervalType={
      if ((this.intervalTypes!=null)&&(!this.intervalTypes.isEmpty))
         this.intervalTypes.last;
     else null;
   }
   /**
    * 获取给定类型的上级市场时段类型
    */
   def getUpperType(itvType:MarketIntervalType):MarketIntervalType={
     val index:Int= this.intervalTypes.indexOf(itvType);
     if (index==0) null else this.intervalTypes(index -1);
   }
   /**
    * 获取给定类型的下级市场时段类型
    */
   def getSubType(itvType:MarketIntervalType):MarketIntervalType={
     val index:Int= intervalTypes.indexOf(itvType);
     if (index==this.intervalTypes.size - 1) null else  intervalTypes(index +1);
   }
   /**
    * 将给定的字符串解码为时段对象。只满足需要将时段持久化为字符串时的特殊需求，一般情况下，时段应按开始和结束时间持久化。
    */
   def decode(code:String):MktInterval=
   {
      def getMatchType:MarketIntervalType={
           val listBuffer:ListBuffer[MarketIntervalType]=new ListBuffer() ;
           for(itvType <- intervalTypes){
               if( itvType.decoder.canDecode(code)) listBuffer += itvType;
           }
           if (listBuffer.size>1) sys.error("严重问题：给定市场时段ID可以被多个时段类型解析");
           if (listBuffer.size==0) sys.error("严重问题：没有时段类型可以解析给定市场时段ID");
           listBuffer(0);
      }
      val decoderType:MarketIntervalType= getMatchType;
      decoderType.decoder.decode(code);
   }
   /**
    * 将给定的时段编码为字符串。将给定的时段对象编码为字符串。只满足需要将时段持久化为字符串时的特殊需求，一般情况下，时段应按开始和结束时间持久化。
    */
   def encode(itv:MktInterval):String=itv.intervalType.decoder.encode(itv);
   /**
    * 获取与给定自然时段完全精准匹配的市场时间段
    */
   def getIntervalAt(timeInterval:TimeInterval):MktInterval={
     if (timeInterval==null) null else 
        this.getIntervalAt(timeInterval.start,timeInterval.end);
   }
   /**
    * 获取与给定自然时段完全精准匹配的市场时间段
    */
   def getIntervalAt(startInclusive:LocalDateTime,endExclusive:LocalDateTime):MktInterval={
     val itvList=for{itvType<-this.intervalTypes
          itv=itvType.getIntervalAt(startInclusive, endExclusive);
          if (itv!=null)
      } yield itv;
      if (itvList.size>1) 
        sys.error("系统时间段类型配置异常，相同自然时间段存在两种不同类型的市场时间段");
      else if(itvList.size==0)  null;
      else itvList(0);
   }
    /**
    * 获得给定时段类型下，在所在交易中心的起始时间段。
    */
  def getOriginInterval(predefType:PredefIntverType.PredefIntverType):MktInterval={
    val itvType=this.getIntervalType(predefType);
    itvType.getOrginInterval;
  }
  /**
   * 获取给定时段类型下，从给定类型时间段从设定的开始时间到包括给定自然时间点时所经历的所有时间段。
   * 
   */
  def getIntervalsTo(predefType:PredefIntverType.PredefIntverType,pointInclusive:LocalDateTime)={
    val itvType=this.getIntervalType(predefType);
    itvType.getIntervalsTo(pointInclusive);
  }
  //for java
  def getIntervalListTo(predefType:PredefIntverType.PredefIntverType,
       pointInclusive:LocalDateTime):java.util.List[MktInterval]=getIntervalsTo(predefType,pointInclusive).asJava;
  /**
   * 获取给定时段类型下，从该类型时间段从设定的开始时间到给定自然时间点之前所经历的所有时间段。
   */
  def getIntervalsUntil(predefType:PredefIntverType.PredefIntverType,pointExclusive:LocalDateTime)={
     val itvType=this.getIntervalType(predefType);
     itvType.getIntervalsUntil(pointExclusive);
  }
  //for java List
  def getIntervalListUntil(predefType:PredefIntverType.PredefIntverType,
       pointInclusive:LocalDateTime):java.util.List[MktInterval]=getIntervalsUntil(predefType,pointInclusive).asJava;
  
   /**
   * 得到给定时段类型下，在给定自然时间段内的市场时间段 
   */
   def getIntervalsBetween(predefType:PredefIntverType.PredefIntverType,startInclusive:LocalDateTime,endExclusive:LocalDateTime)={
      val itvType=this.getIntervalType(predefType);
      itvType.getIntervalsBetween(startInclusive,endExclusive);
  }
   //for java List
  def getIntervalListBetween(predefType:PredefIntverType.PredefIntverType,
          startInclusive:LocalDateTime,
          endExclusive:LocalDateTime):java.util.List[MktInterval]={
       getIntervalsBetween(predefType,startInclusive,endExclusive).asJava
  }
  /**
   * 得到给定时段类型下，包含给定自然时间点的市场时间段 
   */
   def getIntervalInclude(predefType:PredefIntverType.PredefIntverType,pointInclusive:LocalDateTime):MktInterval={
      val itvType=this.getIntervalType(predefType);
      itvType.getIntervalInclude(pointInclusive);
   }
   /**
   * 得到给定时段类型下，与给定自然时间段相交的市场时间段。
   */
  def getIntervalsCross(predefType:PredefIntverType.PredefIntverType,startInclusive:LocalDateTime,endExclusive:LocalDateTime):List[MktInterval]={
     val itvType=this.getIntervalType(predefType);
     itvType.getIntervalsCross(startInclusive,endExclusive);
     
  }
  // for java List
  def getIntervalListCross(predefType:PredefIntverType.PredefIntverType,
          startInclusive:LocalDateTime,
          endExclusive:LocalDateTime):java.util.List[MktInterval]={
       getIntervalsCross(predefType,startInclusive,endExclusive).asJava
  }
    /**
    * 获得给定时段类型下，在所在交易中心的起始时间段。
    */
  def getOriginInterval(itvType:MarketIntervalType):MktInterval={
    itvType.getOrginInterval;
  }
  /**
   * 获取给定时段类型下，从给定类型时间段从设定的开始时间到包括给定自然时间点时所经历的所有时间段。
   * 
   */
  def getIntervalsTo(itvType:MarketIntervalType,pointInclusive:LocalDateTime)={
    itvType.getIntervalsTo(pointInclusive);
  }
  // for java list
  def getIntervalListTo(itvType:MarketIntervalType,
          pointInclusive:LocalDateTime):java.util.List[MktInterval]={
       getIntervalsTo(itvType,pointInclusive).asJava;
  }
  /**
   * 获取给定时段类型下，从该类型时间段从设定的开始时间到给定自然时间点之前所经历的所有时间段。
   */
  def getIntervalsUntil(itvType:MarketIntervalType,pointExclusive:LocalDateTime)={
     itvType.getIntervalsUntil(pointExclusive);
  }
  // for java list
  def getIntervalListUntil(itvType:MarketIntervalType,
          pointExclusive:LocalDateTime):java.util.List[MktInterval]={
       getIntervalsUntil(itvType,pointExclusive).asJava;
  }
   /**
   * 得到给定时段类型下，在给定自然时间段内的市场时间段 
   */
   def getIntervalsBetween(itvType:MarketIntervalType,startInclusive:LocalDateTime,endExclusive:LocalDateTime)={
      itvType.getIntervalsBetween(startInclusive,endExclusive);
  }
     // for java list
  def getIntervalListBetween(itvType:MarketIntervalType,
          startInclusive:LocalDateTime,endExclusive:LocalDateTime):java.util.List[MktInterval]={
       getIntervalsBetween(itvType,startInclusive,endExclusive).asJava;
  } 
  /**
   * 得到给定时段类型下，包含给定自然时间点的市场时间段 
   */
   def getMktIntervalInclude(itvType:MarketIntervalType,pointInclusive:LocalDateTime):MktInterval={
      itvType.getIntervalInclude(pointInclusive);
   }
   /**
   * 得到给定时段类型下，与给定自然时间段相交的市场时间段。
   */
  def getIntervalsCross(itvType:MarketIntervalType,startInclusive:LocalDateTime,endExclusive:LocalDateTime):List[MktInterval]={
     itvType.getIntervalsCross(startInclusive,endExclusive);
  }
     // for java list
  def getIntervalListCross(itvType:MarketIntervalType,
          startInclusive:LocalDateTime,endExclusive:LocalDateTime):java.util.List[MktInterval]={
       getIntervalsCross(itvType,startInclusive,endExclusive).asJava;
  } 
}
 /**
  * 规范了为特定MarketCalendar进行数据持久化操作的行为，该特质实现者
  * 必须知道其所服务的MarketCalendar对象。
  */
 trait MktCalendarRepository
 {
    def  loadRegisteredIntervalTypes:List[MarketIntervalType];
    def  saveNewIntervalType(itvType:MarketIntervalType):Unit;
    def  updateIntervalType(itvType:MarketIntervalType):Unit;
    def loadTradeCenter:TradeCenter;
 }
 /**
  * 为Scala 实现MktCalendarRepository提供了一个虚基类,
  */
 abstract class BaseMktCalendarRepository(val calendar:MktCalendar) extends MktCalendarRepository {
   
 }
 /**
 * 为Java 实现MktCalendarRepository提供了一个虚基类,Java实现可以如下：
 * class JavaRepoImpl extends  BaseRepositoryForJava
 {
	public JavaRepoImpl(MktCalendar calendar)
	{
		super(calendar);
	}
	
	public java.util.List<MarketIntervalType>loadRegisteredIntervalTypeList()
	{
		
		return null;
	}
	public void saveNewIntervalType(MarketIntervalType itvType)
	{
		
	}
	public void updateIntervalType(MarketIntervalType itvType)
	{
		
	}
	public  TradeCenter loadTradeCenter()
	{
		return null;
	}

}
 */
  abstract class  BaseRepositoryForJava(val calendar:MktCalendar) extends MktCalendarRepository{
      protected def  loadRegisteredIntervalTypeList:java.util.List[MarketIntervalType];
      override def  loadRegisteredIntervalTypes:List[MarketIntervalType]=this.loadRegisteredIntervalTypeList.asScala.toList;
      def  saveNewIntervalType(itvType:MarketIntervalType):Unit;
      def  updateIntervalType(itvType:MarketIntervalType):Unit;
      def loadTradeCenter:TradeCenter;
}


 ////////////////以下是关于时间段相关类的定义/////////////////////////////////////////////////////////////////////
/**
 * 定义了所有自然时段对象的行为和属性规范
 */
trait TimeInterval   
{
 /**
   * 时间段的起点。在连续的时间轴上，一个点既是前段的终点，又是下一段的起点，
   * 按照Java惯例start时间点在本时段之内，end时间点不在本时段之内，即：MktInterval=[start,end)
   */
   def start:LocalDateTime;
   /**
    * 时间段的终点。在连续的时间轴上，一个点既是前段的终点，又是下一段的起点，
    * 按照Java惯例start时间点在本时段之内，end时间点不在本时段之内，即：MktInterval=[start,end)
    */
   def end:LocalDateTime;
   def toTuple:(LocalDateTime,LocalDateTime)=(start,end);
   def toDuration:Duration=Duration.between(this.start, this.end);
   def toPeriod:Period=Period.between(this.start.toLocalDate(), this.end.toLocalDate());
     /**
    * 是否在给定自然时间点之后,时段上所有点都在给定点之后。
    */
   def isAfter(point:LocalDateTime):Boolean=this.start.isAfter(point);
      /**
    * 是否在给定时间段之后
    */
   def isAfter(other:TimeInterval):Boolean={
     if (other==null ) false else  this.start.isAfter(other.end);
   }
    /**
    * 是否在给定自然时间点之前，如果true，则时段上所有点都在给定点之前。时段的尾点与给点相同也是true
    */
   def isBefor(point:LocalDateTime):Boolean={!(this.end.isAfter(point))};
    /**
    * 是否在给定时段之前
    */
   def isBefor(other:TimeInterval):Boolean={
     if (other==null)false else this.end.isBefore(other.start);
   }
    /**
    * 是否包括给定的自然时间点
    */
   def isInclude(point:LocalDateTime):Boolean={point.isEqual(this.start)||
       (point.isAfter(this.start)&&point.isBefore(this.end))
   }
     /**
    * 是否包括给定的时间段
    */
   def isInclude(other:TimeInterval):Boolean={
     if (other==null) false  else this.isInclude(other.start,other.end);
   }
   /**
    * 是否包括给定的自然时间段
    */
   def isInclude(startInclusive:LocalDateTime,endExclusive:LocalDateTime):Boolean=(startInclusive.isEqual(this.start)||
       startInclusive.isAfter(this.start))&&(endExclusive.isEqual(this.end)||
           endExclusive.isBefore(this.end));
    /**
    * 是否被给定的自然时间段包括
    */
   def isIncludeBy(other:TimeInterval):Boolean={
     if (other==null) false else this.isIncludeBy(other.start, other.end);
   }
  /**
    * 是否被给定自然时间段包括
    */
   def isIncludeBy(startInclusive:LocalDateTime,endExclusive:LocalDateTime):Boolean=(startInclusive.isBefore(this.start)||
       startInclusive.isEqual(this.start))&&(endExclusive.isEqual(this.end)||
           endExclusive.isAfter(this.end));
   /**
    * 是否被给定自然时间段相交
    */
   def isCrossWith(other:TimeInterval):Boolean={
     if (other==null) false else this.isCrossWith(other.start,other.end);
   }
   /**
    * 是否与给定的时间段相交。
    */
   def isCrossWith(startInclusive:LocalDateTime,endExclusive:LocalDateTime):Boolean=this.isInclude(startInclusive)||
        this.isInclude(endExclusive)||this.isIncludeBy(startInclusive, endExclusive)||this.isInclude(endExclusive, endExclusive);
   /**
    * 只要时间段的开始和结束时间相同，就认为相等。
    */
   override def equals(other:Any)={
        other match {
          case that:MktInterval=>(this.start==that.start)&&(this.end==that.end);
          case _ =>false;
        }
   }
   override def hashCode:Int=(this.start.hashCode()*47+this.end.hashCode())*47;
}
/**
 * 定义了所有市场时段对象的行为和属性规范
 */
trait MktInterval extends TimeInterval with Ordered[MktInterval] with Comparable[MktInterval] 
{
   def encode:String=this.intervalType.decoder.encode(this);
   def intervalType:MarketIntervalType;
   protected  def timePlusByUnit(time:LocalDateTime,setp:Int,unit:MarketIntervalUnit.MarketIntervalUnit):LocalDateTime=unit match {
            case MarketIntervalUnit.MINUTE=>time.plusMinutes(setp)
            case MarketIntervalUnit.HOUR=>time.plusHours(setp)
            case MarketIntervalUnit.DAY=>time.plusDays(setp)
            case MarketIntervalUnit.MONTH=>time.plusMonths(setp)
            case MarketIntervalUnit.YEAR=>time.plusYears(setp) 
   }
   /**
    * 
    * 是否为上级时段的尾部
    */
   def isTail:Boolean=this.end==this.getUpperInterval.end;
   /**
    * 是否为给定级别上级时段的尾部
    */
   def isTail(level:MarketIntervalType):Boolean={
     require((level!=null)&&(level>this.intervalType),"给定级别不是该时段类型的上级类型");
     this.end==this.getUpperInterval(level).end;
   }
   /**
    * 是否为直接上级的头部
    */
   def isHead:Boolean=this.start==this.getUpperInterval.start;
   /**
    * 是否为给定级别上级时段的头部
    */
   def isHead(level:MarketIntervalType):Boolean={
     require((level!=null)&&(level>this.intervalType),"给定级别不是该时段类型的上级类型");
     this.start==this.getUpperInterval(level).start;
   }
   /**
   * 得到后续时段，如果允许超界overUpperBound=true，则永远会返回对象，否则当超出上级时段边界后会抛出异常。
   * 缺省不允许超界
   */
   def next(overUpperBound:Boolean=false):MktInterval={
     val nextStar:LocalDateTime=this.end.plusSeconds(1);
     val nextItv=this.intervalType.getIntervalInclude(nextStar);
     if ((overUpperBound)||(!this.isTail)) nextItv else sys.error("next操作已超时段边界");
   }
   /**
    * 得到指定步长的后续时段列表，如果允许超界overUpperBound=true，则永远会返回指定步长的时段列表，
    * 否则返回在上级时段边界以内的小于或等于指定步长的列表。缺省不允许超界
    */
   def nextIntervals(step:Int,overUpperBound:Boolean=false):List[MktInterval]={ 
      val listBf:ListBuffer[MktInterval]=new ListBuffer();
      for(i<- 1 to step) {
        val nextStart=this.timePlusByUnit(this.start, 
            i*this.intervalType.unitCount, this.intervalType.intervalUnit);
        val itv=this.intervalType.getIntervalInclude(nextStart) ;
        if((overUpperBound)|| itv.start.isBefore(this.getUpperInterval.end)) listBf += itv;
      }
      listBf.toList;
   }
   //for java list
    def nextIntervalList(step:Int,overUpperBound:Boolean=false):java.util.List[MktInterval]={
      this.nextIntervals(step,overUpperBound).asJava;
    }
   /**
    * 得到前继时段，如果允许超界overUpperBound=true，则永远会返回对象，否则当超出上级时段边界后会抛出异常。
   * 缺省不允许超界
    */
   def prior(overUpperBound:Boolean=false):MktInterval={
         val priorStart:LocalDateTime=timePlusByUnit(this.start, - this.intervalType.unitCount ,this.intervalType.intervalUnit);
         if (!priorStart.isBefore(this.intervalType.originTime)){
             val upperItv=this.getUpperInterval;
             if((overUpperBound)||(!priorStart.isBefore(upperItv.start)))
               this.intervalType.getIntervalInclude(priorStart);
             else sys.error("prior操作已超上级时段边界");
         } else sys.error("prior操作已超时段类型起始原点边界");
   }
   /**
    * 
    */
   def priorIntervals(step:Int,overUpperBound:Boolean=false):List[MktInterval]={ 
      val listBf:ListBuffer[MktInterval]=new ListBuffer();
      for(i<- 1 to step ) {
        val priorStart:LocalDateTime=timePlusByUnit(this.start, - (step+1-i)*this.intervalType.unitCount ,this.intervalType.intervalUnit);
        if (!priorStart.isBefore(this.intervalType.originTime)) {
             val itv=this.intervalType.getIntervalInclude(priorStart);
             if((overUpperBound)|| itv.end.isAfter(this.getUpperInterval.start)) listBf += itv;
        }
      }
      listBf.toList;
   }
   //for java list
     def priorIntervalList(step:Int,overUpperBound:Boolean=false):java.util.List[MktInterval]={
      this.priorIntervals(step,overUpperBound).asJava;
    }
   /**
    *得到指定步长的前继时段列表，如果允许超界overUpperBound=true，则永远会返回指定步长的时段列表，
    *否则返回在上级时段边界以内的小于或等于指定步长的列表。缺省不允许超界
    */
   def getUpperInterval:MktInterval=this.getUpperInterval(this.intervalType.getUpperType);
   /**
    * 
    */
   def getUpperInterval(upperType:MarketIntervalType):MktInterval={
      require((upperType !=null)&&(upperType>this.intervalType),"给定的时段类型不是上级时段");
      upperType.getIntervalInclude(this.start);
   }
   /**
    * 
    */
   def getHead(subItvType:MarketIntervalType):MktInterval={
     require((subItvType!=null)&&(subItvType<this.intervalType),"给定的时段类型不是下级时段");
     subItvType.getIntervalInclude(this.start);
   }
   /**
    * 
    */
   def getHead:MktInterval=this.getHead(this.intervalType.getSubType);
   /**
    * 
    */
   def getTail(subItvType:MarketIntervalType):MktInterval={
      require((subItvType!=null)&&(subItvType<this.intervalType),"给定的时段类型不是上级时段");
      val tailSubStart=this.timePlusByUnit(this.end,
             -1 * subItvType.unitCount, subItvType.intervalUnit);
      subItvType.getIntervalInclude(tailSubStart);
   }
   /**
    * 
    */
   def getTail:MktInterval=this.getTail(this.intervalType.getSubType);
   /**
    * 得到所有直接下级时段列表
    */
   def getSubIntervals():List[MktInterval]=this.getSubIntervals(this.intervalType.getSubType);
   //for java list
   def getSubIntervalList():java.util.List[MktInterval]={
      this.getSubIntervals().asJava;
   }
    /**
    * 的到所有给定下级类型的下级时段列表
    */
   def getSubIntervals(subType:MarketIntervalType):List[MktInterval]={
      require((subType!=null)&&(subType<this.intervalType),"给定的时段类型不是下级时段");
      //val lowerItvlStart:LocalDateTime=subType.alaignToLeft(this.start);
      subType.getIntervalsBetween(this.start,this.end);
   }  
   // for java 
   def getSubIntervalList(subType:MarketIntervalType):java.util.List[MktInterval]={
     this.getSubIntervals(subType).asJava;
   }
   /**
    * 得到指定级别的下级时段的数量。
    */
   def getSubIntervalCounts:Int=this.getSubIntervalCounts(this.intervalType.getSubType);
   /**
    * 得到指定级别的下级时段的数量。
    */
   def getSubIntervalCounts(subItvType:MarketIntervalType):Int={
           if (this.intervalType==this.intervalType.mktCalendar.getMinIntervalType) 0
           else {
             if ((subItvType >this.intervalType)||(subItvType.equals(this.intervalType)))
                 sys.error("给定的时段类型不是下级时段");
             val chronoUnitForLitvType:ChronoUnit=MarketIntervalUnit.toChronoUnit(subItvType.intervalUnit);
             val count=(this.start.until(this.end, chronoUnitForLitvType)/ subItvType.unitCount).toInt;
             count;
           }
   }
  
   /**
    * 同类型时间段，时间在前的为小，时间在后的为大。不同类型的时间段不允许比较
    */
   override def compareTo(other:MktInterval):Int={
     if (!(other.intervalType==this.intervalType)) sys.error("不同时段类型的时段无法比较");
     if (this==other) 0
     else if( this.isBefor(other)) -1 
     else 1;
   };
   /**
    * 这是Scala Ordered接口要求实现的方法
    */
   override def compare(other:MktInterval):Int=this.compareTo(other);
   override def toString=this.intervalType.id+" "+this.start.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)+" to "+
                      this.end.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
   
}
/////////////////////////////////////////////////////////////////////////////////////

 /**
 * 定义了时段类型解码/编码器的行为规范
 */
trait IntervalTypeDecoder
{
    def canDecode(code:String):Boolean;
    def decode(code:String):MktInterval;
    def encode(itv:MktInterval):String;
}
/**
 * 定义时段类型解码/编码器工厂对象的行为规范
 */
trait IntervalDecoderFactory
{
  /**
   *  在本系统中，可能不同交易中心（在市场时间领域，有可认为由市场日历所代表）下的不同的市场类型的解码/编码器的实现可能不同，因此需要
   *  给定的市场日历（代表了交易中心）和该市场日历中注册的市场时段类型，就可以得到该日历下，针对该时段类型的解码器。
   */
  def getDecoder(mktCalendar:MktCalendar,intervalTypeId:String):IntervalTypeDecoder;
}

/**
 * 定义了市场时段类型的属性和行为规范
 */
trait MarketIntervalType extends   Ordered[MarketIntervalType] with Comparable[MarketIntervalType] 
{
  def  id:String;
  def  intervalUnit:MarketIntervalUnit.MarketIntervalUnit;
  def  unitCount:Int;
  def  originTime:LocalDateTime
  def  mktCalendar:MktCalendar;
  def  isStop:Boolean;
  def  stopTime:LocalDateTime;
  def  decoder:IntervalTypeDecoder=mktCalendar.decoderFactory.getDecoder(mktCalendar,id);
  def approximateMinutes:Int={ 
          val minutesOfUnit:Int= this.intervalUnit match { 
            case MarketIntervalUnit.MINUTE=>1
            case MarketIntervalUnit.HOUR=>60
            case MarketIntervalUnit.DAY=>1440
            case MarketIntervalUnit.MONTH=>43200
            case MarketIntervalUnit.YEAR=>525600
         };
         this.unitCount*minutesOfUnit;
   }
  /**
   * 
   */
   def toTuple:Tuple2[Int,MarketIntervalUnit.MarketIntervalUnit]=(this.unitCount,this.intervalUnit);
  
    /**
   * 得到本时段类型的直接上级时段类型
   */
  def getUpperType:MarketIntervalType=this.mktCalendar.getUpperType(this);
  /**
   * 得到本时段类型的直接下级时段类型
   */
  def getSubType:MarketIntervalType=this.mktCalendar.getSubType(this);
   /**
    * 某市场时段类型时间轴是从市场时段类型起始时间作为起始点，由按照等距离时段分割点所分割的连续时段构成。
    * 本方法给定任何一个自然时间点，就找到与其最近的左侧分割点。
    */
   def alaignToLeft(time:LocalDateTime):LocalDateTime={
     val chronoUnit=MarketIntervalUnit.toChronoUnit(this.intervalUnit);
     val passUnitCount=this.originTime.until(time, chronoUnit);
     //整除舍余
     val passItvCount=if (passUnitCount>0) passUnitCount / this.unitCount else 0;
     val result=this.originTime.plus(passItvCount * this.unitCount , chronoUnit);
     result;
  }
   /**
    * 获得该类型的时间段的在所在交易中心的起始时间段。
    */
  def getOrginInterval:MktInterval=this.getIntervalInclude(this.originTime);
  /**
   * 获取从该类型时间段从设定的开始时间到包括给定自然时间点时所经历的所有时间段。
   * 
   */
  def getIntervalsTo(pointInclusive:LocalDateTime)={
    val list=this.getIntervalsCross(this.originTime,pointInclusive);
    val lastItv=list.last;
    if (!lastItv.end.isEqual(pointInclusive)) list else {
       val itv=this.getIntervalInclude(pointInclusive);
       list:::itv::Nil;
    }
  }
  //for java list
  def getIntervalListTo(pointInclusive:LocalDateTime):java.util.List[MktInterval]={
    this.getIntervalsTo(pointInclusive).asJava;
  }
  /**
   * 获取从该类型时间段从设定的开始时间到给定自然时间点之前所经历的所有时间段。
   */
  def getIntervalsUntil(pointExclusive:LocalDateTime)={
    val list=this.getIntervalsCross(this.originTime,pointExclusive);
    for{ itv<-list;
         if (itv.isBefor(pointExclusive))
    }
    yield itv;
   
  }
  //for java list
  def getIntervalListUntil(pointExclusive:LocalDateTime):java.util.List[MktInterval]={
    this.getIntervalsUntil(pointExclusive).asJava;
  }
   /**
   * 得到在给定自然时间段内的市场时间段 
   */
   def getIntervalsBetween(startInclusive:LocalDateTime,endExclusive:LocalDateTime)={
      val itvList=this.getIntervalsCross(startInclusive,endExclusive);
      for{
           itv<-itvList
           if (!itv.isBefor(startInclusive))&&(itv.isBefor(endExclusive))
      }
      yield itv;
      
  }
   //for java list
   def getIntervalListBetween(startInclusive:LocalDateTime,
        endExclusive:LocalDateTime):java.util.List[MktInterval]={
       this.getIntervalsBetween(startInclusive, endExclusive).asJava;
   }
   def getIntervalAt(startInclusive:LocalDateTime,endExclusive:LocalDateTime):MktInterval={
     val itvMaybe=this.getOptionIntervalAt(startInclusive,endExclusive);
     itvMaybe match{
            case Some(itv)=> itv;
            case None=>null;
     }
  }
   /**
    * 
    */
  
  def getOptionIntervalAt(startInclusive:LocalDateTime,endExclusive:LocalDateTime):Option[MktInterval]={
    val itvMaybe=this.getIntervalInclude(startInclusive);
    if (!itvMaybe.start.equals(startInclusive)|| !itvMaybe.end.equals(endExclusive))
       None;
    else Some(itvMaybe);
  }
  def getIntervalListCross(startInclusive:LocalDateTime,
          endExclusive:LocalDateTime):java.util.List[MktInterval]={
      this.getIntervalsCross(startInclusive, endExclusive).asJava;
  }
   //-----------------以下两个方法必须由具体实现类实现-----------------------------------------------
  /**
   * 得到包含给定自然时间点的市场时间段 
   */
   def getIntervalInclude(pointInclusive:LocalDateTime):MktInterval;
   /**
   * 得到与给定自然时间段相交的市场时间段。
   */
  def getIntervalsCross(startInclusive:LocalDateTime,endExclusive:LocalDateTime):List[MktInterval];
 
  //------------------------------------------------------
  /**
   * 市场时段类型对象相等的条件必须是开始时间一致，跨越相同步长的时间单位，且实现类相同。
   */
  override def equals(other:Any)=
      {
        other match {
          case that:MarketIntervalType=>(this.originTime==that.originTime)&&(this.unitCount==that.unitCount)&&
                                  (this.intervalUnit==that.intervalUnit)&&(this.getClass==that.getClass );
          case _ =>false;
        }
      }
   override def hashCode:Int=((this.originTime.hashCode()*47+
                                  this.unitCount.hashCode())*47+this.intervalUnit.hashCode())*47;
   override def compareTo(other:MarketIntervalType):Int=this.approximateMinutes - other.approximateMinutes;
   override def compare(other:MarketIntervalType):Int=this.compareTo(other);
   override def toString:String=this.id+":"+this.unitCount.toString()+" "+this.intervalUnit.toString()+" begin at "+this.originTime.toString();
   
   def toXML=
          <MarketIntervalType id={this.id}
                           tradeCenterId={mktCalendar.tradeCenter.mRID}
                           unitCount={unitCount.toString()}
                           intervalUnit={intervalUnit.toString()}
                           originTime={originTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)}
                           isStop={isStop.toString()}
                           stopTime={ if(stopTime!=null) stopTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME) else ""} 
                           /> 
    
 }
/**
 * 所有市场时段类型的抽象基类
 */
abstract class  AbstractMarketIntervalType (val id:String, val mktCalendar:MktCalendar,
            val unitCount:Int, val intervalUnit:MarketIntervalUnit.MarketIntervalUnit,
            val originTime:LocalDateTime, val isStop:Boolean,
            val stopTime:LocalDateTime) extends MarketIntervalType
{        
     require(id!=null); 
     require(intervalUnit!=null,"市场时间间隔类型的时间单位(intervalUnit)参数不允许为空");
     require(unitCount>0,"市场时间间隔类型的时间跨度(intervalLength)必须大于0");
     require(mktCalendar!=null,"市场时间间隔类型的所在的市场日历(MktCalendar)参数不允许为空");
     /**
      * 得到与给定自然时间段相交的市场时间段。
      */
     def getIntervalsCross(startInclusive:LocalDateTime,endExclusive:LocalDateTime):List[MktInterval]={
         def createIntervals(startInclusive:LocalDateTime,endExclusive:LocalDateTime,
            litvType:MarketIntervalType,litvList:ListBuffer[MktInterval]):Unit={
               if (startInclusive.isBefore(endExclusive))  { 
                 val lowerItvl:MktInterval=new DefaultMarketInterval(startInclusive,litvType);
                 //对于连续的时间轴来说，上一段时间的结尾点就是下一段时间的开始点。
                 val startTime:LocalDateTime=lowerItvl.end;
                 //下面语句应实现确保尾递归,使递归的执行效率与普通循环一样
                 createIntervals(startTime,endExclusive,litvType,litvList += lowerItvl);
               }
         }
         val formalStart=this.alaignToLeft(startInclusive);
         val listBuffer=new ListBuffer[MktInterval]();
         createIntervals(formalStart,endExclusive,this,listBuffer);
         listBuffer.toList;   
     }
     /**
      *得到包含给定自然时间点的市场时间段 
      */
     def getIntervalInclude(pointInclusive:LocalDateTime):MktInterval={
         require(!pointInclusive.isBefore(this.originTime));
         val formalStart=this.alaignToLeft(pointInclusive);
         new DefaultMarketInterval(formalStart,this);
     }
    /**
     * 定义底层缺省的市场间隔实现，开发者无需关注
     */
     private final class DefaultMarketInterval( startTime:LocalDateTime,val intervalType:MarketIntervalType) extends MktInterval
     {
       require(!(startTime==null),"市场时间间隔的开始时间(startTime)参数不允许为空");
       require(!(intervalType==null),"市场时间间隔的间隔类型(intervalType)参数不允许为空");
       val start=this.intervalType.alaignToLeft(startTime);
       val end:LocalDateTime=this.timePlusByUnit(this.start,this.intervalType.unitCount,this.intervalType.intervalUnit);
    }   
}
 //////////////////////////////////////////////////////////////////////////////////

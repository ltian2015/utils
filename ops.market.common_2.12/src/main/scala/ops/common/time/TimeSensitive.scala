package ops
package common
package time
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime.MAX

package object time{
  type IntervalTypeDecoder= String=>Option[BusinessIntervalType];
  /**
   * 时段解码器，将给定字符串，解码为时段。
   * 如果解码器不能解码给定的字符串，则返回None，否则返回Some(interval);
   */
  type IntervalDecoder= String=>Option[BusinessInterval];
  type IntervalTypeEncoder= BusinessIntervalType=>String;
  type IntervalEncoder=BusinessInterval=>String;
  
  /**
   * 
   * Boolean型变量为true,表示
   */
  
}
import time._
/**
 * 非周期时间间隔的步进器,用于求出给定的具体非周期性时间间隔下一个或者上一个时间间隔。
 * 非周期性的时间间隔的next和prior具有业务个性化，无法用统一的算法求出，需要另行设定，
 * 每个非周期性的时段类型如果要支持next和prior操作，必须定义NoPeriodicIntervalSteper。
 */
trait  NoPeriodicIntervalStepper{
  def next(npItv:NoPeriodicInterval):NoPeriodicInterval;
  def prior(npItv:NoPeriodicInterval):NoPeriodicInterval;
}

 /**
  * 用来表示所有时间间隔上下文应遵循的规范。从业务上下文中才能得到所有的时间间隔类型，
  * 并且业务上下文知道如何编码和解码上下文内的时间间隔及其类型。
  */

trait BusinessContext
{
    /**
     * 业务上下文所关联的业务信息。该业务信息用于建立业务上下文与有具体业务对象之间的关联信息。
     * 比如，bussinessInfo可以是一个企业或组织的ID。
     */
    def bussinessInfo:String;
    /**
     * 用于描述业务上下文的信息。
     */
    def descripton:String;
    /**
     * 业务上下文中用于解码时间间隔类型的解码器。
     */
    def typeDecoder:IntervalTypeDecoder;
    
    /**
     * 业务上下文中用于编码时间间隔类型的编码器。
     */
    def typeEncoder:IntervalTypeEncoder;
    /**
     * 业务上下文中用于给定时段类型的时间间隔的解码器。
     */
    def intervalDecoder(intervalType:BusinessIntervalType):IntervalDecoder;
    /**
     * 业务上下文中用于给定时段类型于的时间间隔的编码器。
     */
    def intervalEncoder(intervalType:BusinessIntervalType):IntervalEncoder;
    /**
     * 业务上下文中用于给定的非周期性时间间隔类型的步进器。
     */
    def noPeriodicIntervalStepper(intervalType:NoPeriodicIntervalType):NoPeriodicIntervalStepper;
    /**
     * 获取业务上线文中定义的所有时间间隔类型。
     */
    def getAllIntervalTypes:List[BusinessIntervalType];
    /**
     * 将给定的字符串解码为时间间隔类型。
     */
    def decodeType(typeStr:String):Option[BusinessIntervalType]={
      this.typeDecoder(typeStr);
    }
    /**
     * 将给定的字符串解码为时间间隔。
     */
    def decodeInterval(intervalStr:String):Option[BusinessInterval]={
       val itvTypes=this.getAllIntervalTypes;
       val itvOptList=this.getAllIntervalTypes.map(_.decodeInterval(intervalStr)).filter(_.isDefined);
       itvOptList match {
         case Nil=>None;
         case someItv::Nil=>someItv;
         case _=>sys.error("there's multi intervalType can decode given string in the context");
       }
    }
}
/**
 * 业务间隔类型，每种周期性业务都有自己的初始开始时间。
 */
trait BusinessIntervalType{
  /**
   * 业务间隔类型所在的业务上下文。
   */
  def context:BusinessContext;
  /**
   * 将业务周期类型编码为字符串，可以将该字符串视为业务周期类型的唯一标识，即：ID。
   */
  def encode:String={
    val typeEncoder=this.context.typeEncoder;
    if(typeEncoder==null) sys.error("there's no type endcoder defined in the contxt")
    typeEncoder.apply(this);
  }
  /**
   * 业务周期类型的描述。
   */
  def description:String;
  
  /**
   * 将给定的字符串类型解码
   */
  def decodeInterval(intervalStr:String):Option[BusinessInterval]={
     val IntervalDecoder=this.context.intervalDecoder(this);
     if(IntervalDecoder==null) sys.error("there's no interval decoder defined for this interval type  in the contxt");
     IntervalDecoder.apply(intervalStr);
  }
  /**
   * 业务周期的起始时间
   */
  def originTime:LocalDateTime;
  /**
   * 是否是周期性业务间隔
   */
  def isPeriodic:Boolean;
  /**
   * 转换为周期性的业务间隔
   */
  def asPeriodicInterval:PeriodicIntervalType;
  /**
   * 是否为非周期性的业务间隔
   */
  def isNoPeriodic:Boolean;
  /**
   * 转换为非周期的业务间隔
   */
  def asNoPeriodicIntervalType:NoPeriodicIntervalType;
}
/**
 * 非周期性的业务间隔类型，该类型时间间隔有开始和结束时间，但无固定周期，其时间间隔实例是相继的，在时间段上不能有重叠的部分。
 * 如果一个业务对象有一个数据项（Data field）被显式地标注为时间敏感数据（显式使用@TimeSensitive），那么该业务对象就是时间敏感对象。
 * 对于时间敏感对象，它未被标注为时间敏感的数据项的时间周期类型为NoPeriodicIntervalType.
 * 使用NoPeriodicIntervalType相当于按照更新的归档时间做了变更日志。目前，NoPeriodicIntervalType仅用于
 * 这种系统自动处理变更日志的情况。否则，如果有些业务需要每次变更数据都要人为指定开始生效时间和结束时间，以保证能够
 * 创建NoPeriodicIntervalType的对象实例，在实际业务的操作上极为不便， 且很少见，应视为业务的不合理，建议明确固定的业务周期。
 * 
 *
 */
trait NoPeriodicIntervalType extends BusinessIntervalType{
  
  override def isNoPeriodic:Boolean=true;
  override def asNoPeriodicIntervalType:NoPeriodicIntervalType=this;
  override def isPeriodic:Boolean=false;
  override def asPeriodicInterval:PeriodicIntervalType={
    sys.error("can not conervt no periodic interval type to periodic interval type")
  }
}
/**
 *周期性的业务间隔类型，该类型时间间隔有固定的周期。
 */
trait PeriodicIntervalType extends BusinessIntervalType{
  def unitCount:Int;
  def unit:ChronoUnit;
  override def isNoPeriodic:Boolean=false;
  override def asNoPeriodicIntervalType:NoPeriodicIntervalType={
    sys.error("can not conervt  periodic interval type to  no periodic interval type")
  }
  override def isPeriodic:Boolean=true;
  override def asPeriodicInterval:PeriodicIntervalType=this;
  
}
/**
* 业务间隔，与自然的时间价格不同的是，业务间隔有具体的业务间隔类型，
* 而且相同类型的业务间隔是按时间先后顺序发生的，同一类型的业务时段之间不能有重叠交叉。
* 因此，相同业务类型的业务时段之间可以进行比较和排序。时间发生早的时段较小。
*/
trait BusinessInterval extends TimeInterval  with Ordered[BusinessInterval] with Comparable[BusinessInterval] {
   def intervalType:BusinessIntervalType;
   /**
    *获得该时间段的后续时间段。具体的子类可以根据需求实现next的策略，比如按照相同持续周期得到“相接”的下个时段，也可以得到间隔某时长“不相接”的下个时段。
    */
   def next:BusinessInterval;
   /**
    * 获得该时间段的前继时间段。具体的子类可以根据需求实现prior的策略,比如按照相同持续周期得到“相接”的上个时段，也可以得到间隔某时长“不相接”的上个时段。
    */
   def prior:BusinessInterval;
     /**
    * 对时间段进行编码，得到唯一标识时间段的字符串。
    */
   def encode:String;
   
    /**
    * 同类型时间段，时间在前的为小，时间在后的为大。不同类型的时间段不允许比较
    */
   override def compareTo(other:BusinessInterval):Int={
     if (!(other.intervalType==this.intervalType)) sys.error("不同时段类型的时段无法比较");
     if (this==other) 0
     else if( this.isBefor(other)) -1 
     else 1;
   };
   /**
    * 这是Scala Ordered接口要求实现的方法
    */
   override def compare(other:BusinessInterval):Int=this.compareTo(other);
   override def toString=this.intervalType.encode+" "+this.start.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)+" to "+
                      this.end.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
   
}
/**
 * 表示无固定周期的业务时段。
 */
trait NoPeriodicInterval extends BusinessInterval{
  override def intervalType:NoPeriodicIntervalType;
  /**
    *获得该时间段的后续时间段。具体的stepper子类可以根据需求实现next的策略，比如按照相同持续周期得到“相接”的下个时段，也可以得到间隔某时长“不相接”的下个时段。
    */
   override def next:NoPeriodicInterval={
     val stepper=this.intervalType.context.noPeriodicIntervalStepper(this.intervalType);
     if (stepper==null) sys.error("next or porior operation is not supproted!");
     stepper.next(this);
   }
   /**
    * 获得该时间段的前继时间段。具体的stepper子类可以根据需求实现prior的策略,比如按照相同持续周期得到“相接”的上个时段，也可以得到间隔某时长“不相接”的上个时段。
    */
   def prior:NoPeriodicInterval={
      val stepper=this.intervalType.context.noPeriodicIntervalStepper(this.intervalType);
      if (stepper==null) sys.error("next or porior operation is not supproted!");
      stepper.prior(this);
   }
}

/**
 * 表示有固定周期的业务时段。
 */
trait PeriodicInterval extends BusinessInterval{
   override def intervalType:PeriodicIntervalType;
 
}

trait TSV[T]
{
   def value:T;
   def effStartInclsvTime:LocalDateTime;
   def effEndExclsvTime:LocalDateTime;
}
/**
 * trait TS[T]{
    def currentValue:T;
    def valuesIn(startInclusive:LocalDateTime,endExclsvTime:LocalDateTime):List[T];
    def valueAt(timePoint:LocalDateTime):T;
    def newVersion:TSV[T];
    def cycle: BusinessCycle;
}
 * 
 */
import ops.common.annotations._
trait Customer{
   def id:String;
   def name:String;
   @TimeSensitive
   def vipLevel:Int;
   @TimeSensitive
   def kind:Int;
}
trait TimeTag{
 def  timePoint: LocalDateTime;
}
import scala.reflect;
abstract class CustomerDto{ 
   def id:String;
   def name:String;
   @TimeSensitive
   def vipLevel:TSV[Int];
   @TimeSensitive
   def kind:TSV[Int];
}
class DomainCustomer{
  val id:String=null;
  val name:String=null
  val vipLevel:Int=0;
 }

package ops
package common
package time
import java.time.LocalDateTime
import java.time.Duration
import java.time.Period;
/**
 * @author lant
 * TimeInterval表示有具体开始时间和结束时间的时间段，这与java.time.Duration和java.time.Period不同，二者
 * 是表示没有具体开始时间和结束时间的相对时间周期。Period是以Date为单位的时间周期，故本时间段不一定可以得到需要的Period
 * 不跨天的时间段得到的period是0days，而起止时间跨天但实际上少于24小时的时间段得到的period则是1days。
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
   /**
    * 获得该时间段持续的时间。
    */
   def duration:Duration=Duration.between(this.start, this.end); 
   /**
    * 得到估算的以日为单位的周期，因为 Period是以Date为单位的时间周期，故本时间段不一定可以得到需要的Period
    * 不跨天的时间段得到的period是0days，而起止时间跨天但实际上少于24小时的时间段得到的period则是1days。
    */
   def estimatePeriod:Period=Period.between(this.start.toLocalDate(), this.end.toLocalDate());
  
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
          case that:TimeInterval=>(this.start==that.start)&&(this.end==that.end);
          case _ =>false;
        }
   }
   //TODO 需要确认一下LocalDatetime的HashCode方法是否可用
   override def hashCode:Int=(this.start.hashCode()*47+this.end.hashCode())*47;
}
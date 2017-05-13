package actors
{
  import akka.actor._
  import akka.actor.SupervisorStrategy._
  import scala.concurrent.duration._
  import akka.util.Timeout
  import akka.event.LoggingReceive
  import com.typesafe.config.ConfigFactory
  
  object Worker{
    case object Start;
    case object Do;
    final case class Progress(percent:Double);
  }
  /**
  * Listens on progress from the worker and shuts down the system when enough
  * work has been done.
  */
  class Listener extends Actor with ActorLogging {
    import Worker._
  
    // 如果15秒钟内，该actor没有收到任何消息，则系统会自动向该actor发出一个ReceiveTimeout消息，以表明所需要的服务已经不可用了。
    //If we don��t get any progress within 15 seconds then the service is unavailable
    context.setReceiveTimeout(15 seconds)
    def receive = {
      case Progress(percent) =>
        log.info("Current progress: {} %", percent)
        if (percent >= 100.0) {
          log.info("That��s all, shutting down")
          context.system.terminate()
        }
      case ReceiveTimeout =>
        // No progress within 15 seconds, ServiceUnavailable
        log.error("Shutting down due to unavailable service")
        context.system.terminate()
    }
  }
   /**
   * Worker actor 有一个子actor 类型为CounterService
   * Worker actor 处理Start，DO两个消息，Start消息由 主程序发出，类型为Listener的外部 actor发出，
   * DO消息由Worker在处理Start消息时自身发出的。
   * Worker将产生一个类型为Progress的消息，并将该消息发送给Listener
   */

  class Worker extends Actor with ActorLogging
  {
     import Worker._;
     import CounterService._;
     implicit val askTimeout=Timeout(5 seconds);
    /** 通过为Actor指定监视策略来决定如何处理接收到的来自直接的子Actor 的异常。AKKA提供的异常处理策略有两个：
   *  OneForOneStrategy:该策略是只对发出异常的子Actor应用策略中给出的具体处理指令.这里使用了该策略，策略中指定的具体处理指令是Stop，即停止该子 Actor  CounterService.
   *  AllForOneStrategy:该策略是只要有一个子Actor发出异常，就将策略中给出具体的处理指令应用于所有的子Actor。
   * Worker  Actor 负责处理其子Actor CounterService所发出ServiceUnavailable异常，一旦接受到该异常就停止子 CounterService（包括它的所有子Actor）.
   * OneForOneStrategy策略声明如下：
   * case class OneForOneStrategy( maxNrOfRetries: Int = -1, 
   *                              thinTimeRange: Duration = Duration.Inf,
                                  override val loggingEnabled: Boolean = true)
                                  (val decider: SupervisorStrategy.Decider)
                                  extends SupervisorStrategy 
   * OneForOneStrategy策略是SupervisorStrategy的子类， 将偏函数参数`Decider`中给出的故障处理'指令（ Directive）' (Resume, Restart, Stop) 
   * 应用于出现故障的Actor。Decider声明为：type Decider = PartialFunction[Throwable, Directive]它是一个以被抛出的异常（Throwable类型）为参数，
   * 返回一个Directive类型结果的偏函数，在代码中，可以用case表达式作为偏函数的字面量。 Directive（指令）是个封闭特质定义为：sealed trait Directive 
   * 该特质有四个单例对象实现：Resume,Restart，Stop和Escalate，Directive特质中有返回这个四个单例对象的函数：resume,restart,stop和escalate。
   * OneForOneStrategy其他的参数及含义如下：
   * @param maxNrOfRetries 允许子Actor重启（restart）的最大次数限制，如果值为负数，表示没有限制。如果超过这限制仍无法重启子Actor，则停止（ stop）该子Actor。
   * @param withinTimeRange ，表示最大重启次数的持续时间时间窗口限制， 也就是必须在给定的时间窗口内完成重启尝试工作。Duration.Inf表示没有时间窗口限制。
   * @param decider 参数上面已经介绍过了。
   * @param loggingEnabled 参数表示是否将故障计入日志。如果为true表示记录故障日志，缺省值为true。 
   * */

     override val supervisorStrategy = OneForOneStrategy() {
       case _ :CounterService.ServiceUnavailable=>Stop;
     } 
    
     //向worker发出初始的Start消息的sender持续通知进度（progress），这里是Listener，（见主程序代码行 //worker.tell(Start, sender = listener) ）
     var progressListener:Option[ActorRef]=None;
     val counterService=this.context.actorOf(Props[CounterService], name="counter");
     val totalCount=51;
     import context.dispatcher;
     import akka.event.LoggingReceive
     import scala.concurrent.Future;
     def receive=LoggingReceive{
         case Start if progressListener.isEmpty=>
            this.progressListener=Some(sender());
          
            /**Duration.Zero是指schedule函数执行后为任何延迟执行。
             * 1 second 是指每个1秒钟发出一次消息（创建一个新的线程来发出该消息，主线程不阻塞）。
             * self参数表明消息接受者是自己。
             * DO是指发出的消息。
             * 因此，下面这句话是含义是立即向自己发出一个Do消息，然后每隔1秒向自己发出一个Do消息。
             * */

            this.context.system.scheduler.schedule(Duration.Zero, 1 second, self, Do);
           
            /***
             * 接受到一个Do消息，就向counterService发出3个Increment(1)的消息和一个执行进度查询请求的消息，并在得到进度请求的结果后，
             * 将进度消息发送给
             */

          case Do=>
            counterService ! Increment(1);
            counterService ! Increment(1);
            counterService ! Increment(1);
             
        
         // ?操作符函数是由 akka.pattern.ask包中的AskSupport特质中定义的隐式转换类提供的，ask函数的另一种简便写法。

            import akka.pattern.{ ask}
            val f:Future[Any]=(counterService ? GetCurrentCount);
        
         /**
          * Future[T]的map[S] 函数通过接受一个 T=>S 的转化函数，将包含T类型结果的Future[T]转换为包含S类型结果的Future[S]
          */

            val f2:Future[Progress]=f.map({
               case CurrentCount(_,count)=>Progress(100.0 * count / totalCount);
            });
      
      /**通过 import akka.pattern.pipe 得到了一个隐式转换的功能，
       * 将Future转化为PipeableFuture，从而具备pipeTo能力。pipeto将结果转发给一个接受者。   
       * 这里的意思是   Worker 向counterService 发送GetCurrentCount 请求(ask)，
       * counterService将请求结果以Future[Any]类型方式返回给Worker，Worker再将Future[Any]类型的结果的map方法转化为
       * Future[Progress]，然后将Future[Progress]通过akka.pattern.pipe包中的隐式转换PipeToSupport.PipeableFuture类型，调用其pipeTo方法将Future[Progress]发送给
       * progressListener，这里是Start消息的发送者，即：listener（主程序代码行 //worker.tell(Start, sender = listener) ）
      */ 

           import akka.pattern.{ pipe};
            f2.pipeTo(progressListener.get);
     }
  }
  object CounterService {
      final case class Increment(n: Int)
      sealed abstract class GetCurrentCount
      case object GetCurrentCount extends GetCurrentCount
      final case class CurrentCount(key: String, count: Long)
      class ServiceUnavailable(msg: String) extends RuntimeException(msg)
      private case object Reconnect
  }
  /**
   * 
   */
  class CounterService extends Actor{
    import CounterService._;
    import Counter._;
    import Storage._;
    import context.dispatcher // Use this Actors�� Dispatcher as ExecutionContext
    import akka.event.LoggingReceive
     // Restart the storage child when StorageException is thrown.
    // After 3 restarts within 5 seconds it will be stopped.
    override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 3,
           withinTimeRange = 5 seconds) {
           case _: Storage.StorageException =>Restart
    }
    val key = self.path.name
    var storage: Option[ActorRef] = None
    var counter: Option[ActorRef] = None
    var backlog = IndexedSeq.empty[(ActorRef, Any)]
    val MaxBacklog = 10000
    
    override def preStart() {
      initStorage()
    }
    /**
    * The child storage is restarted in case of failure, but after 3 restarts,
    * and still failing it will be stopped. Better to back-off than continuously
    * failing. When it has been stopped we will schedule a Reconnect after a delay.
    * Watch the child so we receive Terminated message when it has been terminated.
    */
    def initStorage() {
      storage = Some(context.watch(context.actorOf(Props[Storage], name = "storage")))
      // Tell the counter, if any, to use the new storage
      counter foreach { _ ! UseStorage(storage) }
      // We need the initial value to be able to operate
      storage.get ! GetMsg(key)
    }
    def receive = LoggingReceive {
      case Entry(k, v) if k == key && counter == None =>
        // Reply from Storage of the initial value, now we can create the Counter
        val c = context.actorOf(Props(classOf[Counter], key, v))
        counter = Some(c)
        // Tell the counter to use current storage
        c ! UseStorage(storage)
        // and send the buffered backlog to the counter
        for ((replyTo, msg) <- backlog) c.tell(msg, sender = replyTo)
        backlog = IndexedSeq.empty
      case msg: Increment => forwardOrPlaceInBacklog(msg)
      case msg: GetCurrentCount => forwardOrPlaceInBacklog(msg)
      case Terminated(actorRef) if Some(actorRef) == storage =>
        // After 3 restarts the storage child is stopped.
        // We receive Terminated because we watch the child, see initStorage.
        storage = None
        // Tell the counter that there is no storage for the moment
        counter foreach { _ ! UseStorage(None) }
        // Try to re-establish storage after while
        context.system.scheduler.scheduleOnce(10 seconds, self, Reconnect)
      case Reconnect =>
        // Re-establish storage after the scheduled delay
        initStorage()
    }
    def forwardOrPlaceInBacklog(msg: Any) {
      // We need the initial value from storage before we can start delegate to
      // the counter. Before that we place the messages in a backlog, to be sent
      // to the counter when it is initialized.
      counter match {
        case Some(c) => c forward msg
        case None =>
          if (backlog.size >= MaxBacklog) 
             throw new ServiceUnavailable(
               "CounterService not available, lack of initial value")
          backlog :+= (sender() -> msg)
      }
    }
  }
  object Counter {
    final case class UseStorage(storage: Option[ActorRef])
  }
  /**
  * The in memory count variable that will send current
  * value to the ��Storage��, if there is any storage
  * available at the moment.
  */
  class Counter(key: String, initialValue: Long) extends Actor {
    import Counter._
    import CounterService._
    import Storage._
    import akka.event.LoggingReceive
    var count = initialValue
    var storage: Option[ActorRef] = None
    def receive = LoggingReceive {
      case UseStorage(s) =>
        storage = s
        storeCount()
      case Increment(n) =>
        count += n
        storeCount()
      case GetCurrentCount =>
        sender() ! CurrentCount(key, count)
    }
    def storeCount() {
      // Delegate dangerous work, to protect our valuable state.
      // We can continue without storage.
      storage foreach { _ ! StoreMsg(Entry(key, count)) }
    }
  }
  
  object Storage{
    final case class Entry(key:String,value:Long);
    final case class StoreMsg(entry:Entry); 
    final case class GetMsg(key:String);
    case class StorageException(msg:String) extends RuntimeException(msg);
  }
  class Storage extends Actor{
    import  Storage._ 
    import akka.event.LoggingReceive
    val db=DummyDB;
      def receive=LoggingReceive {
         case StoreMsg(Entry(key, count)) => db.save(key, count);
         case GetMsg(key)=>this.sender! Entry(key,db.load(key).getOrElse(0L)); 
      };
  }   
  object DummyDB{
    import Storage.StorageException;
    private var db=Map[String,Long]();
    def save(key:String,value:Long):Unit=synchronized{
      if (11<=value&&value>=14)
      {
         throw new StorageException("simulated DB exception");
      }
      
    }
    def load(key:String):Option[Long]=synchronized{
       this.db.get(key);
    }
  }
}
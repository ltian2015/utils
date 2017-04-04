package test.samples
import org.junit.runner.RunWith
import collection.mutable.ArrayStack
import org.scalatest._
/**
 * 使用Scalatest编写“规格测试”风格和理念的测试用例,
 * 如果使用@RunWith(classOf[org.scalatest.junit.JUnitRunner])
 * 则 eclipse IDE中鼠标右键点击测试文件后的弹出菜单中的以下两项均可用
 * Run as -> 1.Junit Test
 * Run as -> 2.Scala Junit Test
 * 
 * 如果不使用@RunWith(classOf[org.scalatest.junit.JUnitRunner])
 * 则只能用scalatest,如下：
 * Run as -> 3.ScalaTest -Suit
 * 
 */
@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class SpecExampleOfScalaTest extends FlatSpec with Matchers {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new ArrayStack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be (2)
    stack.pop() should be (1) 
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new ArrayStack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    } 
  }
}
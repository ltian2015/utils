
package test.samples

import org.scalatest.junit.JUnit3Suite
import org.junit.runner.RunWith
import domain.sample.Foo
/**
*  在Scalatest框架中
 * A <code>Suite</code> that is also a <code>junit.framework.TestCase</code>. 
 *
 * <p>
 * A <code>JUnit3Suite</code> may be run by either JUnit 3 (such as JUnit 3.8) or ScalaTest's runner. You write it the way
 * you write a JUnit 3 <code>TestCase</code>. Tests are methods that start with <code>test</code>, take no parameters, and
 * have a <code>Unit</code> return type. You manage fixtures with methods <code>setUp</code> and <code>tearDown</code>.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.junit.JUnit3Suite
 * import scala.collection.mutable.ListBuffer
 *
 * class BlastFromThePastSuite extends JUnit3Suite {
 *
 *   var sb: StringBuilder = _
 *   var lb: ListBuffer[String] = _
 *
 *   override def setUp(): Unit = {
 *     sb = new StringBuilder("ScalaTest is ")
 *     lb = new ListBuffer[String]
 *   }
 *
 *   def testEasy(): Unit = { // Uses JUnit-style assertions
 *     sb.append("easy!")
 *     assertEquals("ScalaTest is easy!", sb.toString)
 *     assertTrue(lb.isEmpty)
 *     lb += "sweet"
 *   }
 *
 *   def testFun(): Unit = { // Uses ScalaTest assertions
 *     sb.append("fun!")
 *     assert(sb.toString === "ScalaTest is fun!")
 *     assert(lb.isEmpty)
 *   }
 * }
 * </pre>
 * 
 * <p>
 * You can use either JUnit's assertions, inherited from <code>TestCase</code>, or ScalaTest's, inherited from <code>AssertionsForJUnit</code>.
 * </p>
 *
 * <p>
 * When writing JUnit 3 tests in Scala, you should keep in mind that JUnit 3 will not run tests that have a return type other than
 * <code>Unit</code>. Thus it is best to explicitly state the <code>Unit</code> result type, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * def testGoodIdea(): Unit = { // result type will be Unit
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * Instead of this:
 * </p>
 *
 * <pre class="stHighlight">
 * def testBadIdea() = { // result type will be inferred
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * If the <code>testBadIdea</code> method ends in an expression that has a result type other than <code>Unit</code>, the Scala
 * compiler will infer a result type to the <code>testBadIdea</code> method to be the same non-<code>Unit</code> type. As a "result,"
 * JUnit 3 will not discover or run the <code>testBadIdea</code> method at all.
 * </p>
 *ScalaTest也支持行为驱动的开发方式，以人类可读性较好的规格说明书方式来进行测试和验证，见SpecExampleOfScalaTest。
 * 但是其提供的规格描述语义与另外较为流行的specs2 有所不同，关于使用specs2进行规格测试案例，请见specs2test文件。 
 * scalatest 的官网指南见：
 * http://www.scalatest.org/user_guide
 */
@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class FooScalaTest extends JUnit3Suite {
  var foo:Foo=_;
  /**
   * ScalaTest使用此方法专门用于建立测试的初始化环境。
   */
  override def setUp(): Unit = {
     foo = Foo("1","apples",10,"ten apples");
  }
  /**
   * 凡是以test开头，无参数，返回Unit的方法都是被ScalaTest用于测试的方法。
   */
  def testDiv:Unit={
    assert(this.foo.counts==10,"it is count is not10");
    val fooShars=this.foo.div(5);
    assert(fooShars.size==5,"good")
   } 
}

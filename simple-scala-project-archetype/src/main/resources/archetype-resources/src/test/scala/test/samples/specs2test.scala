package test.samples

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.SpecificationWithJUnit
import org.junit._
import org.specs2.matcher._
import org.specs2.matcher.JUnitMustMatchers
import  org.specs2.matcher.JUnitShouldMatchers
import  org.specs2.matcher._
import org.specs2.runner.JUnitRunner

  

/**
 * Sample specification.
 * 
 *   关于使用spec2人类友好语言编写测试用例进行“规格测试” 的资料参见：
 *   https://etorreborre.github.io/specs2/guide/SPECS2-3.8.9/org.specs2.guide.UserGuide.html
 */
@RunWith(classOf[org.specs2.runner.JUnitRunner])
class FooSpec2Test extends   Specification   {
    "The 'Hello world' string" should {
      "contain 11 characters" in {
        "Hello world" must have size(11)
      }
      "start with 'Hello'" in {
        "Hello world" must startWith("Hello")
      }
      "end with 'world'" in {
        "Hello world" must endWith("world")
      }
    }
}

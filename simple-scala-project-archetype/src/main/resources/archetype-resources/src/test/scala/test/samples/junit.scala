package test.samples

import org.junit._
import Assert._
import domain.sample.Foo
/**
 * 在Java中直接使用Junit进行测试，无需依赖其他配合框架。
 * 与Java中使用Junit进行测试的风格相同，在Scala程序员看来，代码量有些大。
 * 使用存粹的Scala测试风格的框架可用ScalaTest，这是基于Junit的Scala风格测试框架，
 * 案例可见本包中的scalatest.scala文件。
 */
@Test
class FooJunitTest {
    @Test
    def fooDiv ={
      val foo=Foo("1","apples",10," 10 apples");
      assertEquals(foo.counts,10);
      val fooShares=foo.div(2);
      assertEquals(fooShares.size,2);
    }
}



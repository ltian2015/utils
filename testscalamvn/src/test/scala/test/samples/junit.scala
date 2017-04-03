package test.samples

import org.junit._
import Assert._
import domain.sample.Foo
@Test
class FooJunitTest {

    @Test
    def fooDiv ={
      val foo=Foo("1","apples",10," 10 apples");
      assertEquals(foo.counts,10);
      val fooShares=foo.div(2);
      assertEquals(fooShares.size,2);
    }

//    @Test
//    def testKO() = assertTrue(false)

}



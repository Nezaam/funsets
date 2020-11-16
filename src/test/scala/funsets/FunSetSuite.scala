package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val p1 = 1
    val p2 = 200
    val p3 = 17

    val s1 = singletonSet(p1)
    val s2 = singletonSet(p2)
    val s3 = singletonSet(p3)
    val s1Unions2Unions3 = union(s1, union(s2, s3))

    def propEvenNumber(x: Int): Boolean = x % 2 == 0
    def propOddNumber(x: Int): Boolean = !propEvenNumber(x)
    def propConvertOddNumberToEvenNumber(x: Int): Int =
      if(x % 2 == 0) x
      else x + 1
  }

  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, p1), "Union 1")
      assert(contains(s, p2), "Union 2")
      assert(!contains(s, p3), "Union 3")
    }
  }

  @Test def `intersect of s2 and s3 should not contain them`: Unit = {
    new TestSets {
      val s = intersect(s2, s3)
      assert(!contains(s, p2), "Intersection 1")
      assert(!contains(s, p3), "Intersection 2")
    }
  }

  @Test def `intersect s1Unions2Unions3 and s3 should contain only s3`: Unit = {
    new TestSets {
      val s = intersect(s1Unions2Unions3, s3)
      assert(!contains(s, p1), "Intersection 1")
      assert(!contains(s, p2), "Intersection 2")
      assert(contains(s, p3), "Intersection 3")
    }
  }

  @Test def `diff of s1Unions2Unions3 and s3 should contain s1 and s2`: Unit = {
    new TestSets {
      val s = diff(s1Unions2Unions3, s3)
      assert(contains(s, p1), "Diff 1")
      assert(contains(s, p2), "Diff 2")
      assert(!contains(s, p3), "Diff 3")
    }
  }

  @Test def `filter s1Unions2Unions3 to get even number`: Unit = {
    new TestSets {
      val s = filter(s1Unions2Unions3, propEvenNumber)
      assert(!contains(s, p1), "Filter 1")
      assert(contains(s, p2), "Filter 2")
      assert(!contains(s, p3), "Filter 3")
    }
  }

  @Test def `forall test s1Unions2Unions3 properties?`: Unit = {
    new TestSets {
      assert(forall(s1Unions2Unions3, (x: Int) => x > 0), "forall > 0")
      assert(!forall(s1Unions2Unions3, propEvenNumber), "forall even number")
      assert(!forall(s1Unions2Unions3, propOddNumber), "forall odd number?")
    }
  }

  @Test def `exists test s1Unions2Unions3 properties?`: Unit = {
    new TestSets {
      assert(exists(s1Unions2Unions3, (x: Int) => x > 100), "exists > 100")
      assert(exists(s1Unions2Unions3, propEvenNumber), "exists even number")
      assert(exists(s1Unions2Unions3, propOddNumber), "exists odd number?")
      assert(exists(s1Unions2Unions3, propOddNumber)
        || exists(s1Unions2Unions3, propEvenNumber), "exists number?")
    }
  }

  @Test def `map test properties`: Unit = {
    new TestSets {
      val s = map(s1Unions2Unions3, propConvertOddNumberToEvenNumber)
      printSet(s)
      assert(!forall(s1Unions2Unions3, propEvenNumber), "forall s1Unions2Unions3 not even number")
      assert(forall(s, propEvenNumber), "forall is even number")
    }
  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

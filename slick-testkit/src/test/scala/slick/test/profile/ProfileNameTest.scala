package slick.test.profile

import org.junit.Test
import org.junit.Assert._
import slick.jdbc.H2Profile
import slick.jdbc.SQLiteProfile

class ProfileNameTest {

  @Test def testProfileNames: Unit = {
    assertEquals("slick.jdbc.H2Profile$", H2Profile.toString)
    assertEquals("slick.jdbc.SQLiteProfile$", SQLiteProfile.toString)
  }
}

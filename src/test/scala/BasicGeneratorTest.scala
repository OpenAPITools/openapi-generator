import com.wordnik.swagger.core.util.JsonUtil
import com.wordnik.swagger.codegen.util._
import com.wordnik.swagger.core.Documentation

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.collection.JavaConverters._
import scala.reflect.BeanProperty

@RunWith(classOf[JUnitRunner])
class BasicGeneratorTest extends FlatSpec with ShouldMatchers {
  val json = ScalaJsonUtil.getJsonMapper

  behavior of "BasicGenerator"
}

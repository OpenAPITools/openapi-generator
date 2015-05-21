import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.mapAsJavaMapConverter
import scala.collection.JavaConverters.seqAsJavaListConverter

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

import com.wordnik.swagger.codegen.examples.ExampleGenerator
import com.wordnik.swagger.models.Model
import com.wordnik.swagger.models.ModelImpl
import com.wordnik.swagger.models.Xml
import com.wordnik.swagger.models.properties.ArrayProperty
import com.wordnik.swagger.models.properties.RefProperty
import com.wordnik.swagger.models.properties.StringProperty

@RunWith(classOf[JUnitRunner])
class ExampleGeneratorTest extends FlatSpec with Matchers {
  val json = "application/json"
  val xml = "application/xml"

  it should "check handling of recursive models" in {
    val nodeType = "Node"
    val ref = new RefProperty(nodeType)
    val node = new ModelImpl().name(nodeType).property("name", new StringProperty())
    node.property("parent", ref)
    node.property("children", new ArrayProperty(ref))
    node.property("wrappedChildren", new ArrayProperty(ref).xml(new Xml().wrapped(true)))
    val pairType = "Pair"
    val pair = new ModelImpl().name(pairType)
    for (item <- Map("first" -> "First", "second" -> "Second")) {
      val property = new RefProperty(nodeType)
      property.setXml(new Xml().name(item._2))
      pair.property(item._1, property);
    }
    val types = scala.collection.mutable.Buffer[String]()
    val expectedTypes = List(json, xml)
    val eg = new ExampleGenerator(Map[String, Model](nodeType -> node, pairType -> pair).asJava)
    for (item <- eg.generate(null, expectedTypes.asJava, new RefProperty(pairType)).asScala) {
      val example = item.get("example")
      item.get("contentType") match {
        case `xml` => {
          types += xml
          example should be ("<Pair>\\n" +
                             "  <Node>\\n" +
                             "    <name>string</name>\\n" +
                             "    <wrappedChildren>\\n" +
                             "    </wrappedChildren>\\n" +
                             "  </Node>\\n" +
                             "  <Node>\\n" +
                             "    <name>string</name>\\n" +
                             "    <wrappedChildren>\\n" +
                             "    </wrappedChildren>\\n" +
                             "  </Node>\\n" +
                             "</Pair>")
        }
        case `json` => {
          types += json
          // TODO - add JSON validation
          example should not be (null)
        }
      }
    }
    types should be (expectedTypes)
  }
}

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.native.Serialization.{read, write}

import com.wordnik.swagger.model._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ModelRefSerializationTest extends FlatSpec with ShouldMatchers {
  implicit val formats = DefaultFormats + new ModelRefSerializer

  it should "deserialize a model ref" in {
    val jsonString = """
    {
      "$ref":"Foo",
      "type":"Bar"
    }
    """
    val json = parse(jsonString)
    json.extract[ModelRef] match {
      case p: ModelRef => {
        p.ref should be ("Foo")
        p.`type` should be ("Bar")
      }
      case _ => fail("expected type ModelRef")
    }
  }

  it should "serialize a model ref" in {
    val ref = ModelRef("Foo", "Bar")
    write(ref) should be ("""{"$ref":"Foo","type":"Bar"}""")
  }
}

@RunWith(classOf[JUnitRunner])
class ModelPropertySerializationTest extends FlatSpec with ShouldMatchers {
  implicit val formats = DefaultFormats + new AllowableValuesSerializer + new ModelPropertySerializer + new ModelRefSerializer

  it should "deserialize a model property with allowable values and ref" in {
    val jsonString = """
    {
      "type":"string",
      "required":false,
      "description":"nice",
      "allowableValues": {
        "valueType":"LIST",
        "values":["1","2","3"]
      },
      "items":{
        "$ref":"Foo",
        "type":"Bar"
      }
    }
    """
    val json = parse(jsonString)
    json.extract[ModelProperty] match {
      case p: ModelProperty => {
        p.`type` should be ("string")
        p.required should be (false)
        p.description should be (Some("nice"))
        p.allowableValues match {
          case e: AllowableListValues => e.values should be (List("1","2","3"))
          case _ => fail("expected allowable values")
        }
        p.items match {
          case Some(e: ModelRef) => {
            e.ref should be ("Foo")
            e.`type` should be ("Bar")
          }
          case _ => fail("expected type ModelProperty")
        }
      }
      case _ => fail("expected type ModelProperty")
    }
  }

  it should "serialize a model property with allowable values and ref" in {
    val p = ModelProperty("string", false, Some("nice"), AllowableListValues(List("a","b")),Some(ModelRef("Foo","Bar")))
    write(p) should be ("""{"type":"string","required":false,"description":"nice","allowableValues":{"valueType":"LIST","values":["a","b"]},"items":{"$ref":"Foo","type":"Bar"}}""")
  }

  it should "deserialize a model property with allowable values" in {
    val jsonString = """
    {
      "type":"string",
      "required":false,
      "description":"nice",
      "allowableValues": {
        "valueType":"LIST",
        "values":["1","2","3"]
      }
    }
    """
    val json = parse(jsonString)
    json.extract[ModelProperty] match {
      case p: ModelProperty => {
        p.`type` should be ("string")
        p.required should be (false)
        p.description should be (Some("nice"))
        p.allowableValues match {
          case e: AllowableListValues => e.values should be (List("1","2","3"))
          case _ => fail("expected allowable values")
        }
      }
      case _ => fail("expected type ModelProperty")
    }
  }

  it should "serialize a model property with allowable values" in {
    val p = ModelProperty("string", false, Some("nice"), AllowableListValues(List("a","b")))
    write(p) should be ("""{"type":"string","required":false,"description":"nice","allowableValues":{"valueType":"LIST","values":["a","b"]}}""")
  }

  it should "deserialize a model property" in {
    val jsonString = """
    {
      "type":"string",
      "required":false,
      "description":"nice",
      "allowableValues": {
        "valueType":"LIST",
        "values":["1","2","3"]
      }
    }
    """
    val json = parse(jsonString)
    json.extract[ModelProperty] match {
      case p: ModelProperty => {
        p.`type` should be ("string")
        p.required should be (false)
        p.description should be (Some("nice"))
      }
      case _ => fail("expected type ModelProperty")
    }
  }

  it should "serialize a model property" in {
    val p = ModelProperty("string", false, Some("nice"))
    write(p) should be ("""{"type":"string","required":false,"description":"nice"}""")
  }
}

@RunWith(classOf[JUnitRunner])
class AllowableValuesSerializersTest extends FlatSpec with ShouldMatchers {
  implicit val formats = DefaultFormats + new AllowableValuesSerializer

  it should "deserialize allowable value list" in {
    val allowableValuesListString = """
    {
      "valueType":"LIST",
      "values":["1","2","3"]
    }
    """
    val json = parse(allowableValuesListString)
    json.extract[AllowableValuesFoo] match {
      case avl: AllowableListValues => {
        avl.valueType should be ("LIST")
        avl.values should be (List("1","2","3"))        
      }
    }
  }

  it should "serialize allowable values list" in {
    val l = AllowableListValues(List("1","2","3"))
    write(l) should be ("""{"valueType":"LIST","values":["1","2","3"]}""")
  }

  it should "deserialize allowable values range" in {
    val allowableValuesRangeString = """
    {
      "valueType":"RANGE",
      "min":"abc",
      "max":3
    }
    """
    val json = parse(allowableValuesRangeString)
    json.extract[AllowableValuesFoo] match {
      case avr: AllowableRangeValues => {
        avr.min should be ("abc")
        avr.max should be ("3")        
      }
      case _ => fail("wrong type returned, should be AllowabeValuesList")
    }
  }

  it should "serialize allowable values range" in {
    val l = AllowableRangeValues("-1", "3")
    write(l) should be ("""{"valueType":"RANGE","min":"-1","max":"3"}""")
  }
}
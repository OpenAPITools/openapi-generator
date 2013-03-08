import com.wordnik.swagger.codegen.util.CoreUtils

import com.wordnik.swagger.model._
import com.wordnik.swagger.codegen.util._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.read

import scala.collection.mutable.LinkedHashMap

@RunWith(classOf[JUnitRunner])
class CoreUtilsTest extends FlatSpec with ShouldMatchers {
  sys.props += "fileMap" -> "src/test/resources/petstore"

  behavior of "CoreUtils"

  it should "verify models are extracted" in {
    val resourceListing = ResourceExtractor.fetchListing("src/test/resources/petstore/resources.json")
    val apis = ApiExtractor.extractApiOperations("src/test/resources/petstore", resourceListing.apis)

    val cu = CoreUtils.extractAllModels(apis)
    cu.size should be (5)

    (cu.keys.toSet & Set("User", "Tag", "Pet", "Category", "Order")).size should be (5)
  }

  it should "verify operation names" in {
    val resourceListing = ResourceExtractor.fetchListing("src/test/resources/petstore/resources.json")
    val apis = ApiExtractor.extractApiOperations("src/test/resources/petstore", resourceListing.apis)

    val petApi = apis.filter(api => api.resourcePath == "/pet").head
    val eps = petApi.apis.map(api => (api.path, api)).toMap
    val ops = eps("/pet.{format}").operations.map(ep => (ep.nickname, ep)).toMap

    ops.size should be (2)

    (ops.keys.toSet & Set("addPet", "updatePet")).size should be (2)
  }

  it should "find required models" in {
  	val apis = CoreUtilsTest.sampleApis1
  	val models = CoreUtils.extractApiModels(apis.head)
  	models.size should be (5)
  }

  it should "find required models from a nested list" in {
    val apis = CoreUtilsTest.sampleApis2
    val models = CoreUtils.extractApiModels(apis.head)
    models.size should be (5)
  }
}

object CoreUtilsTest {
  implicit val formats = SwaggerSerializers.formats

	def sampleApis1 = {
		parse("""
[
  {
    "apiVersion": "0.2",
    "swaggerVersion": "1.1",
    "basePath": "http://api.helloreverb.com/api",
    "resourcePath": "/mysteries",
    "apis": [
      {
        "path": "/mysteries.{format}/{petId}",
        "description": "As the name suggests",
        "operations": [
          {
            "httpMethod": "GET",
            "summary": "You find amazing htings here",
            "responseClass": "DeepMystery",
            "nickname": "getMysteryById",
            "parameters": [
              {
                "name": "id",
                "description": "ID of mystery",
                "paramType": "path",
                "required": true,
                "allowMultiple": false,
                "dataType": "string"
              }
            ]
          }
        ]
      }
    ],
    "models": {
      "MysteryList": {
        "id": "MysteryList",
        "properties": {
          "id": {
            "type": "long"
          },
          "mysteries": {
            "items":{
              "$ref":"Mystery1"
            },
            "type":"Array"
          }
        }
      },
      "DeepMystery": {
        "id": "DeepMystery",
        "properties": {
          "id": {
            "type": "Mystery1"
          },
          "name": {
            "type": "string"
          }
        }
      },
      "Mystery1": {
        "id": "Mystery1",
        "properties": {
          "mystery2": {
            "type": "Mystery2"
          },
          "name": {
            "type": "string"
          }
        }
      },
      "Mystery2": {
        "id": "Mystery2",
        "properties": {
          "mystery3": {
            "type": "Mystery3"
          },
          "name": {
            "type": "string"
          }
        }
      },
      "Mystery3": {
        "id": "Mystery3",
        "properties": {
          "mystery4": {
            "type": "Mystery4"
          },
          "name": {
            "type": "string"
          }
        }
      },
      "Mystery4": {
        "id": "Mystery4",
        "properties": {
          "id": {
            "type": "string"
          },
          "name": {
            "type": "string"
          }
        }
      }
    }
  }
]
				""").extract[List[ApiListing]]
	}

  def sampleApis2 = {
    parse("""
[
  {
    "apiVersion": "0.2",
    "swaggerVersion": "1.1",
    "basePath": "http://api.helloreverb.com/api",
    "resourcePath": "/mysteries",
    "apis": [
      {
        "path": "/mysteries.{format}/{petId}",
        "description": "As the name suggests",
        "operations": [
          {
            "httpMethod": "GET",
            "summary": "You find amazing htings here",
            "responseClass": "MysteryList",
            "nickname": "getMysteryById",
            "parameters": [
              {
                "name": "id",
                "description": "ID of mystery",
                "paramType": "path",
                "required": true,
                "allowMultiple": false,
                "dataType": "string"
              }
            ]
          }
        ]
      }
    ],
    "models": {
      "MysteryList": {
        "id": "MysteryList",
        "properties": {
          "id": {
            "type": "long"
          },
          "mystery1": {
            "type":"Mystery1"
          }
        }
      },
      "Mystery1": {
        "id": "Mystery1",
        "properties": {
          "mystery2": {
            "type": "Mystery2"
          },
          "name": {
            "type": "string"
          }
        }
      },
      "Mystery2": {
        "id": "Mystery2",
        "properties": {
          "mystery3List": {
            "items": {
              "$ref": "Mystery3"
            },
            "type": "List"
          },
          "name": {
            "type": "string"
          }
        }
      },
      "Mystery3": {
        "id": "Mystery3",
        "properties": {
          "mystery4": {
            "type": "Mystery4"
          },
          "name": {
            "type": "string"
          }
        }
      },
      "Mystery4": {
        "id": "Mystery4",
        "properties": {
          "id": {
            "type": "string"
          },
          "name": {
            "type": "string"
          }
        }
      }
    }
  }
]
        """).extract[List[ApiListing]]
  }
}
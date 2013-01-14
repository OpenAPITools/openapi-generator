/**
 *  Copyright 2012 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import com.wordnik.swagger.model._

import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.read

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.io._

@RunWith(classOf[JUnitRunner])
class SwaggerModelTest extends FlatSpec with ShouldMatchers {
  implicit val formats = SwaggerSerializers.formats

	behavior of "Swagger Model"

	it should "deserialize ResourceListing" in {
		val json = Source.fromFile("src/test/resources/petstore/resources.json").mkString
		val listing = parse(json).extract[ResourceListing]

		listing.apiVersion should be ("0.2")
		listing.swaggerVersion should be ("1.1")
		listing.basePath should be ("http://petstore.swagger.wordnik.com/api")
		listing.apis.size should be (3)

		val apis = listing.apis.map(api => (api.path, api.description)).toMap

		apis("/store.{format}") should be ("Operations about store")
		apis("/pet.{format}") should be ("Operations about pets")
		apis("/user.{format}") should be ("Operations about user")
	}

	it should "deserialize ApiListing" in {
		val json = Source.fromFile("src/test/resources/petstore/pet.json").mkString
		val apiListing = parse(json).extract[ApiListing]

		apiListing.apiVersion should be ("0.2")
		apiListing.swaggerVersion should be ("1.1")
		apiListing.basePath should be ("http://petstore.swagger.wordnik.com/api")
		apiListing.resourcePath should be ("/pet")
		apiListing.apis.size should be (4)
		apiListing.models.size should be (3)

		val apiMap = apiListing.apis.map(api => (api.path, api)).toMap
		val petBaseApi = apiMap("/pet.{format}/{petId}")
		petBaseApi.description should be ("Operations about pets")
		petBaseApi.operations.size should be (1)

		val getPetById = petBaseApi.operations.head
		getPetById.httpMethod should be ("GET")
		getPetById.summary should be ("Find pet by ID")
		getPetById.notes should be ("Returns a pet based on ID")
		getPetById.responseClass should be ("Pet")

		getPetById.nickname should be ("getPetById")
		getPetById.parameters.size should be (1)

		val param = getPetById.parameters.head
		param.name should be ("petId")
		param.description should be ("ID of pet that needs to be fetched")
		param.paramType should be ("path")
		param.required should be (true)
		param.allowMultiple should be (false)
		param.dataType should be ("string")

		getPetById.errorResponses.size should be (2)
		val errors = getPetById.errorResponses.map(error => (error.code, error.reason)).toMap

		errors(400) should be ("Invalid ID supplied")
		errors(404) should be ("Pet not found")
	}

	it should "deserialize ApiListing with AllowableValues" in {
		val json = Source.fromFile("src/test/resources/petstore/pet.json").mkString
		val apiListing = parse(json).extract[ApiListing]
		val apiMap = apiListing.apis.map(api => (api.path, api)).toMap
		val petBaseApi = apiMap("/pet.{format}/findByStatus")
		val findPetsByStatus = petBaseApi.operations.head
		val param = findPetsByStatus.parameters.head

		param.name should be ("status")
		param.description should be ("Status values that need to be considered for filter")
		param.paramType should be ("query")
		param.required should be (true)
		param.allowMultiple should be (true)
		param.dataType should be ("string")
		param.allowableValues should not be (null)

		param.allowableValues.isInstanceOf[AllowableListValues] should be (true)
		val allowableValues = param.allowableValues.asInstanceOf[AllowableListValues]
		allowableValues.valueType should be ("LIST")
		allowableValues.values.size should be (3)
		(allowableValues.values.toSet & Set("available", "pending", "sold")).size should be (3)
	}

  it should "maintain model property order when deserializing" in {
    val json = Source.fromFile("src/test/resources/petstore/pet.json").mkString
    val apiListing = parse(json).extract[ApiListing]

    val models = apiListing.models
    models.size should be (3)
    val pet = models("Pet")

    val petProperties = pet.properties.toList

    petProperties.size should be (6)
    petProperties(0)._1 should be ("tags")
    petProperties(1)._1 should be ("id")
    petProperties(2)._1 should be ("category")
    petProperties(3)._1 should be ("status")
    petProperties(4)._1 should be ("name")
    petProperties(5)._1 should be ("photoUrls")
  }

	it should "deserialize models" in {
 		val json = Source.fromFile("src/test/resources/petstore/pet.json").mkString
		val apiListing = parse(json).extract[ApiListing]

		val models = apiListing.models
		models.size should be (3)

		val pet = models("Pet")
		pet.id should be ("Pet")
		pet.properties.size should be (6)

    val properties = pet.properties
		val tags = properties("tags")
		tags.`type` should be ("Array")
		tags.items should not be (None)
		tags.items.get.ref should be (Some("Tag"))

		val id = properties("id")
		// id.`type` shoud be ("long")

		val category = properties("category")
		category.`type` should be ("Category")

		val status = properties("status")
		status.`type` should be ("string")
		status.description should be (Some("pet status in the store"))
		status.allowableValues should not be (null)
		status.allowableValues.isInstanceOf[AllowableListValues] should be (true)
		val allowableValues = status.allowableValues.asInstanceOf[AllowableListValues]
		allowableValues.valueType should be ("LIST")
		(allowableValues.values.toSet & Set("available", "pending", "sold")).size should be (3)
	}
}
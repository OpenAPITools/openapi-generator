import org.openapitools.client._
import org.openapitools.client.api._
import org.openapitools.client.model._
 
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.collection.JavaConverters._
import scala.beans.BeanProperty

@RunWith(classOf[JUnitRunner])
class PetApiTest extends FlatSpec with Matchers {
  behavior of "PetApi"
  val api = new PetApi

  it should "add and fetch a pet" in {
    val pet = Pet(
      Some(1000),
      Some(Category(Some(1), Some("sold"))),
      "dragon",
      (for (i <- (1 to 10)) yield "http://foo.com/photo/" + i).toList,
      Some((for (i <- (1 to 5)) yield org.openapitools.client.model.Tag(Some(i), Some("tag-" + i))).toList),
      Some("lost")
    )

    api.addPet(pet)
    api.getPetById(1000) match {
      case Some(pet) => {
        pet.id should be(Some(1000))
        pet.tags.get.size should be(5)
        pet.status should be(Some("lost"))
        pet.category should not be (null)
        pet.category.get.name should be(Some("sold"))
        pet.name should be("dragon")
        pet.photoUrls.size should be(10)
      }
      case None => fail("didn't find pet created")
    }
  }

  it should "update a pet" in {
    val pet = Pet(
      Some(1000),
      Some(Category(Some(1), Some("sold"))),
      "programmer",
      (for (i <- (1 to 10)) yield "http://foo.com/photo/" + i).toList,
      Some((for (i <- (1 to 5)) yield org.openapitools.client.model.Tag(Some(i), Some("tag-" + i))).toList),
      Some("confused")
    )

    api.addPet(pet)

    api.getPetById(1000) match {
      case Some(pet) => {
        pet.name should be("programmer")
        pet.status should be(Some("confused"))
      }
      case None => fail("didn't find pet created")
    }
    val updatedPet = pet.copy(status = Some("fulfilled"))
    api.updatePet(updatedPet)
    api.getPetById(1000) match {
      case Some(pet) => {
        pet.name should be("programmer")
        pet.status should be(Some("fulfilled"))
      }
      case None => fail("didn't find pet updated")
    }
  }

  it should "find pets by status" in {
    api.findPetsByStatus(List("available")) match {
      case Some(pets) => {
        pets.foreach(pet => pet.status should be("available"))
      }
      case None => fail("didn't find pets by status")
    }
  }

  it should "find pets by tag" in {
    api.findPetsByTags(List("tag1", "tag2")) match {
      case Some(pets) => {
        pets.foreach(pet => {
          val tags = (for (tag <- pet.tags.get) yield tag.name).toSet
                       if ((tags & Set(Some("tag1"), Some("tag2"))).size == 0)
            fail("unexpected tags in " + tags)
        })
      }
      case None => fail("didn't find pets by tag")
    }
  }
}

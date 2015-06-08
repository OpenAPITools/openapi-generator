import io.swagger.client._
import io.swagger.client.api._
import io.swagger.client.model._
 
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
      1000,
      Category(1, "sold"),
      "dragon",
      (for (i <- (1 to 10)) yield "http://foo.com/photo/" + i).toList,
      (for (i <- (1 to 5)) yield io.swagger.client.model.Tag(i, "tag-" + i)).toList,
      "lost"
    )

    api.addPet(pet)
    api.getPetById(1000) match {
      case Some(pet) => {
        pet.id should be(1000)
        pet.tags.size should be(5)
        pet.status should be("lost")
        pet.category should not be (null)
        pet.category.name should be("sold")
        pet.name should be("dragon")
        pet.photoUrls.size should be(10)
      }
      case None => fail("didn't find pet created")
    }
  }

  it should "update a pet" in {
    val pet = Pet(
      1000,
      Category(1, "sold"),
      "programmer",
      (for (i <- (1 to 10)) yield "http://foo.com/photo/" + i).toList,
      (for (i <- (1 to 5)) yield io.swagger.client.model.Tag(i, "tag-" + i)).toList,
      "confused"
    )

    api.addPet(pet)

    api.getPetById(1000) match {
      case Some(pet) => {
        pet.name should be("programmer")
        pet.status should be("confused")
      }
      case None => fail("didn't find pet created")
    }
    val updatedPet = pet.copy(status = "fulfilled")
    api.updatePet(updatedPet)
    api.getPetById(1000) match {
      case Some(pet) => {
        pet.name should be("programmer")
        pet.status should be("fulfilled")
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
    println("finding by tags")
    api.findPetsByTags(List("tag1", "tag2")) match {
      case Some(pets) => {
        pets.foreach(pet => {
          val tags = (for (tag <- pet.tags) yield tag.name).toSet
          if ((tags & Set("tag1", "tag2")).size == 0)
            fail("unexpected tags in " + tags)
        })
      }
      case None => fail("didn't find pets by tag")
    }
  }
}

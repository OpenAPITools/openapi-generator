import com.wordnik.petstore.api._
import com.wordnik.petstore.model._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.collection.JavaConversions._
import scala.reflect.BeanProperty

@RunWith(classOf[JUnitRunner])
class PetApiTest extends FlatSpec with ShouldMatchers {
  behavior of "PetApi"
  val api = new PetApi

  it should "fetch a pet" in {
    api.getPetById("1") match {
      case Some(pet) => {
        pet should not be (null)
        pet.id should be(1)
      }
      case None => fail("didn't find pet 1")
    }
  }

  it should "add a new pet" in {
    val pet = new Pet
    pet.id = 1000

    pet.tags = (for (i <- (1 to 5)) yield {
      val tag = new Tag
      tag.id = i; tag.name = "tag-" + i; tag
    }).toList
    pet.status = "lost"
    pet.category = {
      val category = new Category; category.id = 1; category.name = "sold"
      category
    }
    pet.name = "dragon"
    pet.photoUrls = (for (i <- (1 to 10)) yield {
      "http://foo.com/photo/" + i
    }).toList

    api.addPet(pet)
    api.getPetById("1000") match {
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
    val pet = new Pet
    pet.id = 1000

    pet.name = "programmer"
    pet.status = "confused"
    api.addPet(pet)

    api.getPetById("1000") match {
      case Some(pet) => {
        pet.name should be("programmer")
        pet.status should be("confused")
      }
      case None => fail("didn't find pet created")
    }
    pet.status = "fulfilled"
    api.updatePet(pet)
    api.getPetById("1000") match {
      case Some(pet) => {
        pet.name should be("programmer")
        pet.status should be("fulfilled")
      }
      case None => fail("didn't find pet updated")
    }
  }

  it should "find pets by status" in {
    api.findPetsByStatus("available") match {
      case Some(pets) => {
        pets.foreach(pet => pet.status should be("available"))
      }
      case None => fail("didn't find pets by status")
    }
  }

  it should "find pets by tag" in {
    println("finding by tags")
    api.findPetsByTags("tag1,tag2") match {
      case Some(pets) => {
/*        pets.foreach(pet => {
          val tags = (for (tag <- pet.tags) yield tag.name).toSet
          println("checking tags " + tags)
          if ((tags & Set("tag1", "tag2")).size == 0) fail("unexpected tags in " + tags)
        })
*/
      }
      case None => //fail("didn't find pets by tag")
    }
  }
}
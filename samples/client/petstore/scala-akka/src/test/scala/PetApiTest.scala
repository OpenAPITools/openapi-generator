import akka.actor.ActorSystem
import org.junit.runner.RunWith
import org.openapitools.client._
import org.openapitools.client.api._
import org.openapitools.client.core.{ ApiInvoker, ApiKeyValue }
import org.openapitools.client.model._
import org.scalatest.Inspectors._
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PetApiTest extends AsyncFlatSpec with Matchers {

  private implicit val system: ActorSystem = ActorSystem()

  behavior of "PetApi"
  val api = PetApi()
  val invoker = ApiInvoker()
  private implicit val apiKey: ApiKeyValue = ApiKeyValue("special-key")

  it should "add and fetch a pet" in {
    val petId = 1000
    val createdPet = Pet(
      Some(petId),
      Some(Category(Some(1), Some("sold"))),
      "dragon",
      (for (i <- 1 to 10) yield "http://foo.com/photo/" + i).toList,
      Some((for (i <- 1 to 5) yield org.openapitools.client.model.Tag(Some(i), Some("tag-" + i))).toList),
      Some(PetEnums.Status.Sold)
    )

    val addPetRequest = api.addPet(createdPet)
    val getPetRequest = api.getPetById(petId)

    for {
      addResponse <- invoker.execute(addPetRequest)
      response <- invoker.execute(getPetRequest)
    } yield {
      addResponse.code should be(200)

      response.code should be(200)
      val pet = response.content

      pet should have(
        'id (createdPet.id),
        'status (createdPet.status),
        'category (createdPet.category),
        'name (createdPet.name)
      )
      pet.tags should not be empty
      pet.tags.get should contain theSameElementsInOrderAs createdPet.tags.get
      pet.photoUrls should contain theSameElementsInOrderAs createdPet.photoUrls
    }
  }

  it should "update a pet" in {
    val petId = Math.random().toInt
    val createdPet = Pet(
      Some(petId),
      Some(Category(Some(1), Some("sold"))),
      "programmer",
      (for (i <- 1 to 10) yield "http://foo.com/photo/" + i).toList,
      Some((for (i <- 1 to 5) yield org.openapitools.client.model.Tag(Some(i), Some("tag-" + i))).toList),
      Some(PetEnums.Status.Available)
    )

    for {
      _ <- invoker.execute(api.addPet(createdPet))
      pet: core.ApiResponse[Pet] <- invoker.execute(api.getPetById(petId))
      updatedPet = pet.content.copy(status = Some(PetEnums.Status.Sold))
      _ <- invoker.execute(api.updatePet(updatedPet))
      updatedRequested <- invoker.execute(api.getPetById(petId))
    } yield {
      pet.content.name should be("programmer")
      pet.content.status should be(Some(PetEnums.Status.Available))

      updatedRequested.content.name should be("programmer")
      updatedRequested.content.status should be(Some(PetEnums.Status.Sold))
    }
  }

  it should "find pets by status" in {
    val request = api.findPetsByStatus(List("available"))

    invoker
      .execute(request)
      .map { apiResponse =>
        apiResponse.code should be(200)
        val pets = apiResponse.content
        pets should not be empty

        forAll(pets) { pet =>
          pet.status should contain("available")
        }
      }
  }

  it should "find pets by tag" in {
    val request = api.findPetsByTags(List("tag1", "tag2"))

    invoker
      .execute(request)
      .map { apiResponse =>
        apiResponse.code should be(200)

        val pets = apiResponse.content
        pets should not be empty

        forAll(pets) { pet =>
          val tagNames = pet.tags.toList.flatten.map(_.name).collect { case Some(name) => name }
          tagNames should contain atLeastOneOf("tag1", "tag2")
        }
      }
  }
}


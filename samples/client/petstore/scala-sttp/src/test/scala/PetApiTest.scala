import org.junit.runner.RunWith
import org.openapitools.client.api._
import org.openapitools.client.core.{ApiInvoker, ApiKeyValue, SttpSerializer}
import org.openapitools.client.model._
import org.scalatest.Inspectors._
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import sttp.client.HttpURLConnectionBackend

@RunWith(classOf[JUnitRunner])
class PetApiTest extends AsyncFlatSpec with Matchers {

  implicit val sttpSerializer = new SttpSerializer
  implicit val backend = HttpURLConnectionBackend()

  val api = new PetApi("https://petstore3.swagger.io/api/v3")

  implicit val apiKey = ApiKeyValue("api-key")

  import ApiInvoker._

  behavior of "PetApi"

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

    backend
    addPetRequest.result
    val pet = getPetRequest.result

    pet should have(
      'id(createdPet.id),
      'status(createdPet.status),
      'category(createdPet.category),
      'name(createdPet.name)
    )
    pet.tags should not be empty
    pet.tags.get should contain theSameElementsInOrderAs createdPet.tags.get
    pet.photoUrls should contain theSameElementsInOrderAs createdPet.photoUrls
  }

  it should "update a pet" in {
    val petId = (Math.random() * 1000000000).toLong
    val createdPetObj = Pet(
      Some(petId),
      Some(Category(Some(1), Some("sold"))),
      "programmer",
      (for (i <- 1 to 10) yield "http://foo.com/photo/" + i).toList,
      Some((for (i <- 1 to 5) yield org.openapitools.client.model.Tag(Some(i), Some("tag-" + i))).toList),
      Some(PetEnums.Status.Available)
    )

    val createdPet = api.addPet(createdPetObj).result
    val pet = api.getPetById(createdPet.id.get).result
    val updatedPetObj = pet.copy(status = Some(PetEnums.Status.Sold), name = "developer")
    val updatedPet = api.updatePet(updatedPetObj).result
    val updatedRequested = api.getPetById(createdPet.id.get).result

    pet.name should be("programmer")
    pet.status should be(Some(PetEnums.Status.Available))

    updatedPet.name should be("developer")
    updatedPet.status should be(Some(PetEnums.Status.Sold))

    updatedRequested.name should be("developer")
    updatedRequested.status should be(Some(PetEnums.Status.Sold))

  }

  it should "find pets by status" in {
    val pets = api.findPetsByStatus(List("available")).result
    pets should not be empty


    forAll(pets.toList) { pet =>
      pet.status should contain(PetEnums.Status.Available)
    }
  }

  it should "find pets by tag" in {
    val pets = api.findPetsByTags(List("tag1", "tag2")).result
    pets should not be empty

    forAll(pets.toList) { pet =>
      val tagNames = pet.tags.toList.flatten.map(_.name).collect { case Some(name) => name }
      tagNames should contain atLeastOneOf("tag1", "tag2")
    }
  }

}
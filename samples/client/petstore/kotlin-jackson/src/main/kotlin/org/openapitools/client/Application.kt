package org.openapitools

import org.openapitools.client.apis.PetApi
import org.openapitools.client.apis.StoreApi
import org.openapitools.client.models.Category
import org.openapitools.client.models.Pet
import org.openapitools.client.models.Tag

fun main() {
    println("<top>.main")
    val inventory = StoreApi().getInventory()
    println("Inventory : $inventory")
    val pet = Pet(name = "Elliot", photoUrls = listOf<String>("https://jameshooverstudios.com/wp-content/uploads/2015/04/Majestic-Dog-Photography-Elliot-Nov-5-2014.jpg", "https://express-images.franklymedia.com/6616/sites/981/2020/01/22105725/Elliott.jpg"), id = 123456453, category = Category(id = 13259476, name = "dog"), tags = listOf<Tag>(Tag(id = 194093, name = "Elliot")), status = Pet.Status.AVAILABLE)
    PetApi().addPet(pet)
    val elliot = PetApi().getPetById(123456453)
    println("Elliot : $elliot")
    assert(pet == elliot)
    println("<bottom>.main")

}

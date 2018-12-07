package org.openapitools.client.api;

import java.util.List;
import java.util.function.Consumer;

import org.openapitools.client.model.*;
import net.java.html.json.Model;
import net.java.html.json.ModelOperation;
import net.java.html.json.OnReceive;
import net.java.html.json.Property;

@ModelOperation
@Model(className = "PetApi", targetId = "", properties = {
    @Property(name="url", type=String.class )
})
public class PetApiConnector {
    // Add a new pet to the store
    @OnReceive(method = "POST", data=Pet.class,  url = "{url}/pet")
    public static void addPet( PetApi model, Pet pet, Consumer<Throwable> onError) {

    }

    // Deletes a pet
    @OnReceive(method = "DELETE",  url = "{url}/pet/{petId}")
    public static void deletePet( PetApi model, Consumer<Throwable> onError) {

    }

    // Finds Pets by status
    @OnReceive(method = "GET",  url = "{url}/pet/findByStatus")
    public static void findPetsByStatus( PetApi model,List<Pet> result,Consumer<List<Pet>> onSuccess, Consumer<Throwable> onError) {

    }

    // Finds Pets by tags
    @OnReceive(method = "GET",  url = "{url}/pet/findByTags")
    public static void findPetsByTags( PetApi model,List<Pet> result,Consumer<List<Pet>> onSuccess, Consumer<Throwable> onError) {

    }

    // Find pet by ID
    @OnReceive(method = "GET",  url = "{url}/pet/{petId}")
    public static void getPetById( PetApi model, Pet result,Consumer< Pet> onSuccess, Consumer<Throwable> onError) {

    }

    // Update an existing pet
    @OnReceive(method = "PUT", data=Pet.class,  url = "{url}/pet")
    public static void updatePet( PetApi model, Pet pet, Consumer<Throwable> onError) {

    }

    // Updates a pet in the store with form data
    @OnReceive(method = "POST",  url = "{url}/pet/{petId}")
    public static void updatePetWithForm( PetApi model, Consumer<Throwable> onError) {

    }

    // uploads an image
    @OnReceive(method = "POST",  url = "{url}/pet/{petId}/uploadImage")
    public static void uploadFile( PetApi model, Pet result,Consumer< Pet> onSuccess, Consumer<Throwable> onError) {

    }

}

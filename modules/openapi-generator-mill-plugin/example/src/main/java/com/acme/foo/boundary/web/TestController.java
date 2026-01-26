package com.acme.foo.boundary.web;

import com.acme.foo.boundary.web.api.PetApi;
import com.acme.foo.boundary.web.model.ModelApiResponse;
import com.acme.foo.boundary.web.model.Pet;
import jakarta.enterprise.context.RequestScoped;

import java.io.InputStream;
import java.util.List;

@RequestScoped
public class TestController implements PetApi {
    @Override
    public Pet addPet(Pet pet) {
        return null;
    }

    @Override
    public void deletePet(Long petId, String apiKey) {

    }

    @Override
    public List<Pet> findPetsByStatus(List<String> status) {
        return List.of();
    }

    @Override
    public List<Pet> findPetsByTags(List<String> tags) {
        return List.of();
    }

    @Override
    public Pet getPetById(Long petId) {
        return null;
    }

    @Override
    public Pet updatePet(Pet pet) {
        return null;
    }

    @Override
    public void updatePetWithForm(Long petId, String name, String status) {

    }

    @Override
    public ModelApiResponse uploadFile(Long petId, String additionalMetadata, InputStream _fileInputStream) {
        return null;
    }
}

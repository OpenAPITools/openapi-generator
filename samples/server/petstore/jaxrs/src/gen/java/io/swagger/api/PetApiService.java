package io.swagger.api;

import com.sun.jersey.core.header.FormDataContentDisposition;
import io.swagger.api.*;
import io.swagger.api.NotFoundException;
import io.swagger.model.Pet;

import javax.ws.rs.core.Response;
import java.util.List;

public abstract class PetApiService {

    public abstract Response updatePet(Pet body)
            throws NotFoundException;

    public abstract Response addPet(Pet body)
            throws NotFoundException;

    public abstract Response findPetsByStatus(List<String> status)
            throws NotFoundException;

    public abstract Response findPetsByTags(List<String> tags)
            throws NotFoundException;

    public abstract Response getPetById(Long petId)
            throws NotFoundException;

    public abstract Response updatePetWithForm(String petId, String name, String status)
            throws NotFoundException;

    public abstract Response deletePet(String apiKey, Long petId)
            throws NotFoundException;

    public abstract Response uploadFile(Long petId, String additionalMetadata, FormDataContentDisposition fileDetail)
            throws NotFoundException;

}

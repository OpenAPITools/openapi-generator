package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import org.wso2.msf4j.formparam.FormDataParam;
import org.wso2.msf4j.formparam.FileInfo;

import java.io.File;
import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;


public abstract class PetApiService {
    public abstract Response addPet(Pet body
 ) throws NotFoundException;
    public abstract Response deletePet(Long petId
 ,String apiKey
 ) throws NotFoundException;
    public abstract Response findPetsByStatus(List<String> status
 ) throws NotFoundException;
    public abstract Response findPetsByTags(List<String> tags
 ) throws NotFoundException;
    public abstract Response getPetById(Long petId
 ) throws NotFoundException;
    public abstract Response updatePet(Pet body
 ) throws NotFoundException;
    public abstract Response updatePetWithForm(Long petId
 ,String name
 ,String status
 ) throws NotFoundException;
    public abstract Response uploadFile(Long petId
 ,String additionalMetadata
 ,InputStream fileInputStream, FileInfo fileDetail
 ) throws NotFoundException;
}

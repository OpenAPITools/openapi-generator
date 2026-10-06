package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.wso2.msf4j.formparam.FileInfo;

import java.util.List;
import org.openapitools.model.Pet;
import java.util.Set;

import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public abstract class PetApiService {
    public abstract Response addPet(Pet body
 ) throws NotFoundException;
    public abstract Response deletePet(Long petId
 ,String apiKey
 ) throws NotFoundException;
    public abstract Response findPetsByStatus(List<String> status
 ) throws NotFoundException;
    public abstract Response findPetsByTags(Set<String> tags
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
 ,InputStream _fileInputStream, FileInfo _fileDetail
 ) throws NotFoundException;
}

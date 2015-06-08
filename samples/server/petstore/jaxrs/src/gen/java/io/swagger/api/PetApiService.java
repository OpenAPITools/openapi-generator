package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import com.sun.jersey.multipart.FormDataParam;

import io.swagger.model.Pet;
import java.io.File;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Response;

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
    
        public abstract Response updatePetWithForm(String petId,String name,String status)
        throws NotFoundException;
    
        public abstract Response deletePet(String apiKey,Long petId)
        throws NotFoundException;
    
        public abstract Response uploadFile(Long petId,String additionalMetadata,FormDataContentDisposition fileDetail)
        throws NotFoundException;
    
    }

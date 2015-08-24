package io.swagger.handler;

import io.swagger.inflector.models.RequestContext;
import io.swagger.inflector.models.ResponseContext;
import javax.ws.rs.core.Response.Status;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import java.io.File;
import java.util.List;

import io.swagger.model.*;

import io.swagger.model.Pet;
import java.io.File;

public class PetController  {

    public ResponseContext updatePet(RequestContext request ,Pet body)
    {
        return new ResponseContext().status(Status.INTERNAL_SERVER_ERROR).entity( "Not implemented" );
    }
    public ResponseContext addPet(RequestContext request ,Pet body)
    {
        return new ResponseContext().status(Status.INTERNAL_SERVER_ERROR).entity( "Not implemented" );
    }
    public ResponseContext findPetsByStatus(RequestContext request ,List<String> status)
    {
        return new ResponseContext().status(Status.INTERNAL_SERVER_ERROR).entity( "Not implemented" );
    }
    public ResponseContext findPetsByTags(RequestContext request ,List<String> tags)
    {
        return new ResponseContext().status(Status.INTERNAL_SERVER_ERROR).entity( "Not implemented" );
    }
    public ResponseContext getPetById(RequestContext request ,Long petId)
    {
        return new ResponseContext().status(Status.INTERNAL_SERVER_ERROR).entity( "Not implemented" );
    }
    public ResponseContext updatePetWithForm(RequestContext request ,String petId,String name,String status)
    {
        return new ResponseContext().status(Status.INTERNAL_SERVER_ERROR).entity( "Not implemented" );
    }
    public ResponseContext deletePet(RequestContext request ,Long petId,String apiKey)
    {
        return new ResponseContext().status(Status.INTERNAL_SERVER_ERROR).entity( "Not implemented" );
    }
    public ResponseContext uploadFile(RequestContext request ,Long petId,String additionalMetadata,FormDataContentDisposition fileDetail)
    {
        return new ResponseContext().status(Status.INTERNAL_SERVER_ERROR).entity( "Not implemented" );
    }
}


package controllers;

import java.io.File;
import apimodels.Pet;

import io.swagger.annotations.*;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.IOException;
import swagger.SwaggerUtils;
import javafx.util.Pair;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;


@Api(value = "Pet", description = "the Pet API")
public class PetApiController extends Controller {

    private PetApiControllerImp imp;
    private ObjectMapper mapper;

    @Inject
    private PetApiController(PetApiControllerImp imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    @ApiOperation(value = "Add a new pet to the store", notes = "", authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
    @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
    @AuthorizationScope(scope = "read:pets", description = "read your pets")
    })
    }, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 405, message = "Invalid input") })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "body", value = "Pet object that needs to be added to the store", dataType = "apimodels.Pet", paramType = "body")
    })
    public Result addPet() throws IOException {
        JsonNode nodebody = request().body().asJson();
        Pet body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Pet.class);
        
        } else {
            body = null;
        }
        imp.addPet(body);
        
        return ok();
    }

    @ApiOperation(value = "Deletes a pet", notes = "", authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
    @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
    @AuthorizationScope(scope = "read:pets", description = "read your pets")
    })
    }, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 400, message = "Invalid pet value") })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "api_key", value = "", dataType = "String", paramType = "header")
    })
    public Result deletePet(@ApiParam(value = "Pet id to delete", required = true ) Long petId)  {
        String valueapiKey = request().getHeader("api_key");
        String apiKey;
        if (valueapiKey != null) {
            apiKey = (String)valueapiKey;
        
        } else {
            apiKey = "";
        }
        imp.deletePet(petId, apiKey);
        
        return ok();
    }

    @ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma separated strings", response = Pet.class, responseContainer = "List", authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
    @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
    @AuthorizationScope(scope = "read:pets", description = "read your pets")
    })
    }, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 200, message = "successful operation", response = Pet.class), 
    @ApiResponse(code = 400, message = "Invalid status value", response = Pet.class) })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "status", value = "Status values that need to be considered for filter", defaultValue = "available", dataType = "List<String>", paramType = "query")
    })
    public Result findPetsByStatus()  {
        //TODO: Maybe implement this in the future if we can support collection in the body params: see bug in swagger-play: https://github.com/swagger-api/swagger-play/issues/130
        //TODO: Tt seems it is not detected that it's a list based on the collectionFormat field?
        //WIP when both bugs will be fixed
        //List<Pair> statusPair = SwaggerUtils.parameterToPairs("multi", "status", request().getQueryString("status"));
        List<String> status = new ArrayList<String>();
        //for (Pair pair : statusPair) {
        //    status.add(pair.getValue());
        //}
        List<Pet> obj = imp.findPetsByStatus(status);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiOperation(value = "Finds Pets by tags", notes = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", response = Pet.class, responseContainer = "List", authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
    @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
    @AuthorizationScope(scope = "read:pets", description = "read your pets")
    })
    }, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 200, message = "successful operation", response = Pet.class), 
    @ApiResponse(code = 400, message = "Invalid tag value", response = Pet.class) })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "tags", value = "Tags to filter by", dataType = "List<String>", paramType = "query")
    })
    public Result findPetsByTags()  {
        //TODO: Maybe implement this in the future if we can support collection in the body params: see bug in swagger-play: https://github.com/swagger-api/swagger-play/issues/130
        //TODO: Tt seems it is not detected that it's a list based on the collectionFormat field?
        //WIP when both bugs will be fixed
        //List<Pair> tagsPair = SwaggerUtils.parameterToPairs("multi", "tags", request().getQueryString("tags"));
        List<String> tags = new ArrayList<String>();
        //for (Pair pair : tagsPair) {
        //    tags.add(pair.getValue());
        //}
        List<Pet> obj = imp.findPetsByTags(tags);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiOperation(value = "Find pet by ID", notes = "Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions", response = Pet.class, authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
    @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
    @AuthorizationScope(scope = "read:pets", description = "read your pets")
    }),
    @Authorization(value = "api_key")
    }, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 200, message = "successful operation", response = Pet.class), 
    @ApiResponse(code = 400, message = "Invalid ID supplied", response = Pet.class), 
    @ApiResponse(code = 404, message = "Pet not found", response = Pet.class) })
    @ApiImplicitParams({
        
    })
    public Result getPetById(@ApiParam(value = "ID of pet that needs to be fetched", required = true ) Long petId)  {
        Pet obj = imp.getPetById(petId);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiOperation(value = "Update an existing pet", notes = "", authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
    @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
    @AuthorizationScope(scope = "read:pets", description = "read your pets")
    })
    }, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 400, message = "Invalid ID supplied"), 
    @ApiResponse(code = 404, message = "Pet not found"), 
    @ApiResponse(code = 405, message = "Validation exception") })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "body", value = "Pet object that needs to be added to the store", dataType = "apimodels.Pet", paramType = "body")
    })
    public Result updatePet() throws IOException {
        JsonNode nodebody = request().body().asJson();
        Pet body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Pet.class);
        
        } else {
            body = null;
        }
        imp.updatePet(body);
        
        return ok();
    }

    @ApiOperation(value = "Updates a pet in the store with form data", notes = "", authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
    @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
    @AuthorizationScope(scope = "read:pets", description = "read your pets")
    })
    }, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 405, message = "Invalid input") })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "name", value = "Updated name of the pet", dataType = "String", paramType = "form"),
        @ApiImplicitParam(name = "status", value = "Updated status of the pet", dataType = "String", paramType = "form")
    })
    public Result updatePetWithForm(@ApiParam(value = "ID of pet that needs to be updated", required = true ) String petId)  {
        String valuename = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("name"))[0];
        String name;
        if (valuename != null) {
            name = (String)valuename;
        
        } else {
            name = "";
        }
        String valuestatus = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("status"))[0];
        String status;
        if (valuestatus != null) {
            status = (String)valuestatus;
        
        } else {
            status = "";
        }
        imp.updatePetWithForm(petId, name, status);
        
        return ok();
    }

    @ApiOperation(value = "uploads an image", notes = "", authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
    @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
    @AuthorizationScope(scope = "read:pets", description = "read your pets")
    })
    }, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 0, message = "successful operation") })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "additionalMetadata", value = "Additional data to pass to server", dataType = "String", paramType = "form"),
        @ApiImplicitParam(name = "file", value = "file to upload", dataType = "apimodels.File", paramType = "form")
    })
    public Result uploadFile(@ApiParam(value = "ID of pet to update", required = true ) Long petId)  {
        String valueadditionalMetadata = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("additionalMetadata"))[0];
        String additionalMetadata;
        if (valueadditionalMetadata != null) {
            additionalMetadata = (String)valueadditionalMetadata;
        
        } else {
            additionalMetadata = "";
        }
        Http.MultipartFormData.FilePart file = request().body().asMultipartFormData().getFile("file");
                imp.uploadFile(petId, additionalMetadata, file);
        
        return ok();
    }
}

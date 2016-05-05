package io.swagger.api;

import io.swagger.model.*;

import io.swagger.model.Pet;
import java.io.File;

import java.util.concurrent.Callable;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.Authorization;
import io.swagger.annotations.AuthorizationScope;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

import static org.springframework.http.MediaType.*;

@Controller
@RequestMapping(value = "/pet", produces = {APPLICATION_JSON_VALUE})
@Api(value = "/pet", description = "the pet API")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.SpringMVCServerCodegen", date = "2016-05-03T13:42:56.594+02:00")
public interface PetApi {

  @ApiOperation(value = "Add a new pet to the store", notes = "", response = Void.class, authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
      @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
      @AuthorizationScope(scope = "read:pets", description = "read your pets")
      })
  })
  @ApiResponses(value = { 
    @ApiResponse(code = 405, message = "Invalid input") })
  @RequestMapping(value = "", 
    produces = { "application/json", "application/xml" }, 
    consumes = { "application/json", "application/xml" },
    method = RequestMethod.POST)
  default Callable<ResponseEntity<Void>> addPet(

@ApiParam(value = "Pet object that needs to be added to the store"  ) @RequestBody Pet body
)
      throws NotFoundException {
      // do some magic!
      return () -> new ResponseEntity<Void>(HttpStatus.OK);
  }


  @ApiOperation(value = "Deletes a pet", notes = "", response = Void.class, authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
      @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
      @AuthorizationScope(scope = "read:pets", description = "read your pets")
      })
  })
  @ApiResponses(value = { 
    @ApiResponse(code = 400, message = "Invalid pet value") })
  @RequestMapping(value = "/{petId}", 
    produces = { "application/json", "application/xml" }, 
    
    method = RequestMethod.DELETE)
  default Callable<ResponseEntity<Void>> deletePet(
@ApiParam(value = "Pet id to delete",required=true ) @PathVariable("petId") Long petId

,
    
@ApiParam(value = ""  ) @RequestHeader(value="api_key", required=false) String apiKey

)
      throws NotFoundException {
      // do some magic!
      return () -> new ResponseEntity<Void>(HttpStatus.OK);
  }


  @ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma seperated strings", response = Pet.class, responseContainer = "List", authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
      @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
      @AuthorizationScope(scope = "read:pets", description = "read your pets")
      })
  })
  @ApiResponses(value = { 
    @ApiResponse(code = 200, message = "successful operation"),
    @ApiResponse(code = 400, message = "Invalid status value") })
  @RequestMapping(value = "/findByStatus", 
    produces = { "application/json", "application/xml" }, 
    
    method = RequestMethod.GET)
  default Callable<ResponseEntity<List<Pet>>> findPetsByStatus(@ApiParam(value = "Status values that need to be considered for filter", defaultValue = "available") @RequestParam(value = "status", required = false, defaultValue="available") List<String> status


)
      throws NotFoundException {
      // do some magic!
      return () -> new ResponseEntity<List<Pet>>(HttpStatus.OK);
  }


  @ApiOperation(value = "Finds Pets by tags", notes = "Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.", response = Pet.class, responseContainer = "List", authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
      @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
      @AuthorizationScope(scope = "read:pets", description = "read your pets")
      })
  })
  @ApiResponses(value = { 
    @ApiResponse(code = 200, message = "successful operation"),
    @ApiResponse(code = 400, message = "Invalid tag value") })
  @RequestMapping(value = "/findByTags", 
    produces = { "application/json", "application/xml" }, 
    
    method = RequestMethod.GET)
  default Callable<ResponseEntity<List<Pet>>> findPetsByTags(@ApiParam(value = "Tags to filter by") @RequestParam(value = "tags", required = false) List<String> tags


)
      throws NotFoundException {
      // do some magic!
      return () -> new ResponseEntity<List<Pet>>(HttpStatus.OK);
  }


  @ApiOperation(value = "Find pet by ID", notes = "Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions", response = Pet.class, authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
      @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
      @AuthorizationScope(scope = "read:pets", description = "read your pets")
      }),
    @Authorization(value = "api_key")
  })
  @ApiResponses(value = { 
    @ApiResponse(code = 200, message = "successful operation"),
    @ApiResponse(code = 400, message = "Invalid ID supplied"),
    @ApiResponse(code = 404, message = "Pet not found") })
  @RequestMapping(value = "/{petId}", 
    produces = { "application/json", "application/xml" }, 
    
    method = RequestMethod.GET)
  default Callable<ResponseEntity<Pet>> getPetById(
@ApiParam(value = "ID of pet that needs to be fetched",required=true ) @PathVariable("petId") Long petId

)
      throws NotFoundException {
      // do some magic!
      return () -> new ResponseEntity<Pet>(HttpStatus.OK);
  }


  @ApiOperation(value = "Update an existing pet", notes = "", response = Void.class, authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
      @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
      @AuthorizationScope(scope = "read:pets", description = "read your pets")
      })
  })
  @ApiResponses(value = { 
    @ApiResponse(code = 400, message = "Invalid ID supplied"),
    @ApiResponse(code = 404, message = "Pet not found"),
    @ApiResponse(code = 405, message = "Validation exception") })
  @RequestMapping(value = "", 
    produces = { "application/json", "application/xml" }, 
    consumes = { "application/json", "application/xml" },
    method = RequestMethod.PUT)
  default Callable<ResponseEntity<Void>> updatePet(

@ApiParam(value = "Pet object that needs to be added to the store"  ) @RequestBody Pet body
)
      throws NotFoundException {
      // do some magic!
      return () -> new ResponseEntity<Void>(HttpStatus.OK);
  }


  @ApiOperation(value = "Updates a pet in the store with form data", notes = "", response = Void.class, authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
      @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
      @AuthorizationScope(scope = "read:pets", description = "read your pets")
      })
  })
  @ApiResponses(value = { 
    @ApiResponse(code = 405, message = "Invalid input") })
  @RequestMapping(value = "/{petId}", 
    produces = { "application/json", "application/xml" }, 
    consumes = { "application/x-www-form-urlencoded" },
    method = RequestMethod.POST)
  default Callable<ResponseEntity<Void>> updatePetWithForm(
@ApiParam(value = "ID of pet that needs to be updated",required=true ) @PathVariable("petId") String petId

,
    


@ApiParam(value = "Updated name of the pet" ) @RequestPart(value="name", required=false)  String name
,
    


@ApiParam(value = "Updated status of the pet" ) @RequestPart(value="status", required=false)  String status
)
      throws NotFoundException {
      // do some magic!
      return () -> new ResponseEntity<Void>(HttpStatus.OK);
  }


  @ApiOperation(value = "uploads an image", notes = "", response = Void.class, authorizations = {
    @Authorization(value = "petstore_auth", scopes = {
      @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
      @AuthorizationScope(scope = "read:pets", description = "read your pets")
      })
  })
  @ApiResponses(value = { 
    @ApiResponse(code = 200, message = "successful operation") })
  @RequestMapping(value = "/{petId}/uploadImage", 
    produces = { "application/json", "application/xml" }, 
    consumes = { "multipart/form-data" },
    method = RequestMethod.POST)
  default Callable<ResponseEntity<Void>> uploadFile(
@ApiParam(value = "ID of pet to update",required=true ) @PathVariable("petId") Long petId

,
    


@ApiParam(value = "Additional data to pass to server" ) @RequestPart(value="additionalMetadata", required=false)  String additionalMetadata
,
    

@ApiParam(value = "file detail") @RequestPart("file") MultipartFile file
)
      throws NotFoundException {
      // do some magic!
      return () -> new ResponseEntity<Void>(HttpStatus.OK);
  }

}

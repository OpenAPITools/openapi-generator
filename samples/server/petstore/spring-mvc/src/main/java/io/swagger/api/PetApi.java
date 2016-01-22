package io.swagger.api;

import io.swagger.model.*;

import io.swagger.model.Pet;
import java.io.File;
import io.swagger.model.ApiResponse;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
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
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.SpringMVCServerCodegen", date = "2016-01-22T15:27:38.634-06:00")
public class PetApi {
  

  @ApiOperation(value = "Update an existing pet", notes = "", response = Void.class)
  @io.swagger.annotations.ApiResponses(value = { 
    @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied"),
    @io.swagger.annotations.ApiResponse(code = 404, message = "Pet not found"),
    @io.swagger.annotations.ApiResponse(code = 405, message = "Validation exception") })
  @RequestMapping(value = "", 
    produces = { "application/xml", "application/json" }, 
    consumes = { "application/json", "application/xml" },
    method = RequestMethod.PUT)
  public ResponseEntity<Void> updatePet(

@ApiParam(value = "Pet object that needs to be added to the store" ,required=true ) @RequestBody Pet body
)
      throws NotFoundException {
      // do some magic!
      return new ResponseEntity<Void>(HttpStatus.OK);
  }

  

  @ApiOperation(value = "Add a new pet to the store", notes = "", response = Void.class)
  @io.swagger.annotations.ApiResponses(value = { 
    @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input") })
  @RequestMapping(value = "", 
    produces = { "application/xml", "application/json" }, 
    consumes = { "application/json", "application/xml" },
    method = RequestMethod.POST)
  public ResponseEntity<Void> addPet(

@ApiParam(value = "Pet object that needs to be added to the store" ,required=true ) @RequestBody Pet body
)
      throws NotFoundException {
      // do some magic!
      return new ResponseEntity<Void>(HttpStatus.OK);
  }

  

  @ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma separated strings", response = Pet.class, responseContainer = "List")
  @io.swagger.annotations.ApiResponses(value = { 
    @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation"),
    @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid status value") })
  @RequestMapping(value = "/findByStatus", 
    produces = { "application/xml", "application/json" }, 
    
    method = RequestMethod.GET)
  public ResponseEntity<List<Pet>> findPetsByStatus(@ApiParam(value = "Status values that need to be considered for filter", required = true) @RequestParam(value = "status", required = true) List<String> status


)
      throws NotFoundException {
      // do some magic!
      return new ResponseEntity<List<Pet>>(HttpStatus.OK);
  }

  

  @ApiOperation(value = "Finds Pets by tags", notes = "Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", response = Pet.class, responseContainer = "List")
  @io.swagger.annotations.ApiResponses(value = { 
    @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation"),
    @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid tag value") })
  @RequestMapping(value = "/findByTags", 
    produces = { "application/xml", "application/json" }, 
    
    method = RequestMethod.GET)
  public ResponseEntity<List<Pet>> findPetsByTags(@ApiParam(value = "Tags to filter by", required = true) @RequestParam(value = "tags", required = true) List<String> tags


)
      throws NotFoundException {
      // do some magic!
      return new ResponseEntity<List<Pet>>(HttpStatus.OK);
  }

  

  @ApiOperation(value = "Find pet by ID", notes = "Returns a single pet", response = Pet.class, authorizations = {
    @Authorization(value = "api_key")
  })
  @io.swagger.annotations.ApiResponses(value = { 
    @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation"),
    @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied"),
    @io.swagger.annotations.ApiResponse(code = 404, message = "Pet not found") })
  @RequestMapping(value = "/{petId}", 
    produces = { "application/xml", "application/json" }, 
    
    method = RequestMethod.GET)
  public ResponseEntity<Pet> getPetById(
@ApiParam(value = "ID of pet to return",required=true ) @PathVariable("petId") Long petId

)
      throws NotFoundException {
      // do some magic!
      return new ResponseEntity<Pet>(HttpStatus.OK);
  }

  

  @ApiOperation(value = "Updates a pet in the store with form data", notes = "", response = Void.class)
  @io.swagger.annotations.ApiResponses(value = { 
    @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input") })
  @RequestMapping(value = "/{petId}", 
    produces = { "application/xml", "application/json" }, 
    consumes = { "application/x-www-form-urlencoded" },
    method = RequestMethod.POST)
  public ResponseEntity<Void> updatePetWithForm(
@ApiParam(value = "ID of pet that needs to be updated",required=true ) @PathVariable("petId") Long petId

,
    


@ApiParam(value = "Updated name of the pet" ) @RequestPart(value="name", required=false)  String name
,
    


@ApiParam(value = "Updated status of the pet" ) @RequestPart(value="status", required=false)  String status
)
      throws NotFoundException {
      // do some magic!
      return new ResponseEntity<Void>(HttpStatus.OK);
  }

  

  @ApiOperation(value = "Deletes a pet", notes = "", response = Void.class)
  @io.swagger.annotations.ApiResponses(value = { 
    @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied"),
    @io.swagger.annotations.ApiResponse(code = 404, message = "Pet not found") })
  @RequestMapping(value = "/{petId}", 
    produces = { "application/xml", "application/json" }, 
    
    method = RequestMethod.DELETE)
  public ResponseEntity<Void> deletePet(
@ApiParam(value = "Pet id to delete",required=true ) @PathVariable("petId") Long petId

,
    
@ApiParam(value = ""  ) @RequestHeader(value="apiKey", required=false) String apiKey

)
      throws NotFoundException {
      // do some magic!
      return new ResponseEntity<Void>(HttpStatus.OK);
  }

  

  @ApiOperation(value = "uploads an image", notes = "", response = ApiResponse.class)
  @io.swagger.annotations.ApiResponses(value = { 
    @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation") })
  @RequestMapping(value = "/{petId}/uploadImage", 
    produces = { "application/json" }, 
    consumes = { "multipart/form-data" },
    method = RequestMethod.POST)
  public ResponseEntity<ApiResponse> uploadFile(
@ApiParam(value = "ID of pet to update",required=true ) @PathVariable("petId") Long petId

,
    


@ApiParam(value = "Additional data to pass to server" ) @RequestPart(value="additionalMetadata", required=false)  String additionalMetadata
,
    

@ApiParam(value = "file detail") @RequestPart("file") MultipartFile file
)
      throws NotFoundException {
      // do some magic!
      return new ResponseEntity<ApiResponse>(HttpStatus.OK);
  }

  
}

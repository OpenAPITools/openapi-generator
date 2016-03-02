package io.swagger.api;

import io.swagger.model.*;


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
@RequestMapping(value = "/pet?testing_byte_array=true", produces = {APPLICATION_JSON_VALUE})
@Api(value = "/pet?testing_byte_array=true", description = "the pet?testing_byte_array=true API")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.SpringMVCServerCodegen", date = "2016-02-26T13:59:02.543Z")
public interface PettestingByteArraytrueApi {
  

  @ApiOperation(value = "Fake endpoint to test byte array in body parameter for adding a new pet to the store", notes = "", response = Void.class, authorizations = {
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
  default Callable<ResponseEntity<Void>> addPetUsingByteArray(

@ApiParam(value = "Pet object in the form of byte array"  ) @RequestBody byte[] body
)
      throws NotFoundException {
      // do some magic!
      return () -> new ResponseEntity<Void>(HttpStatus.OK);
  }

  
}

package io.swagger.api;

import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;
import org.springframework.core.io.Resource;

import io.swagger.annotations.*;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;

import java.util.List;

import javax.validation.constraints.*;
import javax.validation.Valid;

@Controller
public class PetApiController implements PetApi {
    public ResponseEntity<Void> addPet(@ApiParam(value = "Pet object that needs to be added to the store" ,required=true )  @Valid @RequestBody Pet body,
        @RequestHeader("Accept") String accept) {
        // do some magic!
        return new ResponseEntity<Void>(HttpStatus.OK);
    }

    public ResponseEntity<Void> deletePet(@ApiParam(value = "Pet id to delete",required=true ) @PathVariable("petId") Long petId,
        @ApiParam(value = "" ) @RequestHeader(value="api_key", required=false) String apiKey,
        @RequestHeader("Accept") String accept) {
        // do some magic!
        return new ResponseEntity<Void>(HttpStatus.OK);
    }

    public ResponseEntity<List<Pet>> findPetsByStatus( @NotNull @ApiParam(value = "Status values that need to be considered for filter", required = true, allowableValues = "available, pending, sold") @RequestParam(value = "status", required = true) List<String> status,
        @RequestHeader("Accept") String accept) throws IOException {
        // do some magic!
        
        ObjectMapper objectMapper = new ObjectMapper();

        if (accept != null && accept.contains("application/xml")) {
            return new ResponseEntity<List<Pet>>(objectMapper.readValue("<Pet>  <id>123456789</id>  <name>doggie</name>  <photoUrls>    <photoUrls>aeiou</photoUrls>  </photoUrls>  <tags>  </tags>  <status>aeiou</status></Pet>",List.class), HttpStatus.OK);
        }

        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<List<Pet>>(objectMapper.readValue("[ {  \"photoUrls\" : [ \"aeiou\" ],  \"name\" : \"doggie\",  \"id\" : 0,  \"category\" : {    \"name\" : \"aeiou\",    \"id\" : 6  },  \"tags\" : [ {    \"name\" : \"aeiou\",    \"id\" : 1  } ],  \"status\" : \"available\"} ]",List.class), HttpStatus.OK);
        }

        return new ResponseEntity<List<Pet>>(HttpStatus.OK);
    }

    public ResponseEntity<List<Pet>> findPetsByTags( @NotNull @ApiParam(value = "Tags to filter by", required = true) @RequestParam(value = "tags", required = true) List<String> tags,
        @RequestHeader("Accept") String accept) throws IOException {
        // do some magic!
        
        ObjectMapper objectMapper = new ObjectMapper();

        if (accept != null && accept.contains("application/xml")) {
            return new ResponseEntity<List<Pet>>(objectMapper.readValue("<Pet>  <id>123456789</id>  <name>doggie</name>  <photoUrls>    <photoUrls>aeiou</photoUrls>  </photoUrls>  <tags>  </tags>  <status>aeiou</status></Pet>",List.class), HttpStatus.OK);
        }

        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<List<Pet>>(objectMapper.readValue("[ {  \"photoUrls\" : [ \"aeiou\" ],  \"name\" : \"doggie\",  \"id\" : 0,  \"category\" : {    \"name\" : \"aeiou\",    \"id\" : 6  },  \"tags\" : [ {    \"name\" : \"aeiou\",    \"id\" : 1  } ],  \"status\" : \"available\"} ]",List.class), HttpStatus.OK);
        }

        return new ResponseEntity<List<Pet>>(HttpStatus.OK);
    }

    public ResponseEntity<Pet> getPetById(@ApiParam(value = "ID of pet to return",required=true ) @PathVariable("petId") Long petId,
        @RequestHeader("Accept") String accept) throws IOException {
        // do some magic!
        
        ObjectMapper objectMapper = new ObjectMapper();

        if (accept != null && accept.contains("application/xml")) {
            return new ResponseEntity<Pet>(objectMapper.readValue("<Pet>  <id>123456789</id>  <name>doggie</name>  <photoUrls>    <photoUrls>aeiou</photoUrls>  </photoUrls>  <tags>  </tags>  <status>aeiou</status></Pet>",Pet.class), HttpStatus.OK);
        }

        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<Pet>(objectMapper.readValue("{  \"photoUrls\" : [ \"aeiou\" ],  \"name\" : \"doggie\",  \"id\" : 0,  \"category\" : {    \"name\" : \"aeiou\",    \"id\" : 6  },  \"tags\" : [ {    \"name\" : \"aeiou\",    \"id\" : 1  } ],  \"status\" : \"available\"}",Pet.class), HttpStatus.OK);
        }

        return new ResponseEntity<Pet>(HttpStatus.OK);
    }

    public ResponseEntity<Void> updatePet(@ApiParam(value = "Pet object that needs to be added to the store" ,required=true )  @Valid @RequestBody Pet body,
        @RequestHeader("Accept") String accept) {
        // do some magic!
        return new ResponseEntity<Void>(HttpStatus.OK);
    }

    public ResponseEntity<Void> updatePetWithForm(@ApiParam(value = "ID of pet that needs to be updated",required=true ) @PathVariable("petId") Long petId,
        @ApiParam(value = "Updated name of the pet") @RequestPart(value="name", required=false)  String name,
        @ApiParam(value = "Updated status of the pet") @RequestPart(value="status", required=false)  String status,
        @RequestHeader("Accept") String accept) {
        // do some magic!
        return new ResponseEntity<Void>(HttpStatus.OK);
    }

    public ResponseEntity<ModelApiResponse> uploadFile(@ApiParam(value = "ID of pet to update",required=true ) @PathVariable("petId") Long petId,
        @ApiParam(value = "Additional data to pass to server") @RequestPart(value="additionalMetadata", required=false)  String additionalMetadata,
        @ApiParam(value = "file detail") @RequestPart("file") MultipartFile file,
        @RequestHeader("Accept") String accept) throws IOException {
        // do some magic!
        
        ObjectMapper objectMapper = new ObjectMapper();

        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<ModelApiResponse>(objectMapper.readValue("{  \"code\" : 0,  \"type\" : \"aeiou\",  \"message\" : \"aeiou\"}",ModelApiResponse.class), HttpStatus.OK);
        }

        return new ResponseEntity<ModelApiResponse>(HttpStatus.OK);
    }

}

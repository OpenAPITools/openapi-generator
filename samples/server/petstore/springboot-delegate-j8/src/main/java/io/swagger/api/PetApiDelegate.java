package io.swagger.api;

import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;
import org.springframework.core.io.Resource;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.annotations.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;
import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Optional;

/**
 * A delegate to be called by the {@link PetApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */

public interface PetApiDelegate {

    Logger log = LoggerFactory.getLogger(PetApi.class);

    default Optional<ObjectMapper> getObjectMapper() {
        return Optional.empty();
    }

    default Optional<HttpServletRequest> getRequest() {
        return Optional.empty();
    }

    default Optional<String> getAcceptHeader() {
        return getRequest().map(r -> r.getHeader("Accept"));
    }

    /**
     * @see PetApi#addPet
     */
    default ResponseEntity<Void> addPet(Pet body) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default PetApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see PetApi#deletePet
     */
    default ResponseEntity<Void> deletePet(Long petId,
        String apiKey) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default PetApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see PetApi#findPetsByStatus
     */
    default ResponseEntity<List<Pet>> findPetsByStatus(List<String> status) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
            if (getAcceptHeader().get().contains("application/xml")) {
                try {
                    return new ResponseEntity<>(getObjectMapper().get().readValue("<Pet>  <id>123456789</id>  <name>doggie</name>  <photoUrls>    <photoUrls>aeiou</photoUrls>  </photoUrls>  <tags>  </tags>  <status>aeiou</status></Pet>", List.class), HttpStatus.NOT_IMPLEMENTED);
                } catch (IOException e) {
                    log.error("Couldn't serialize response for content type application/xml", e);
                    return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
                }
            }
            if (getAcceptHeader().get().contains("application/json")) {
                try {
                    return new ResponseEntity<>(getObjectMapper().get().readValue("[ {  \"tags\" : [ {    \"id\" : 1,    \"name\" : \"name\"  }, {    \"id\" : 1,    \"name\" : \"name\"  } ],  \"id\" : 0,  \"category\" : {    \"id\" : 6,    \"name\" : \"name\"  },  \"status\" : \"available\",  \"name\" : \"doggie\",  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ]}, {  \"tags\" : [ {    \"id\" : 1,    \"name\" : \"name\"  }, {    \"id\" : 1,    \"name\" : \"name\"  } ],  \"id\" : 0,  \"category\" : {    \"id\" : 6,    \"name\" : \"name\"  },  \"status\" : \"available\",  \"name\" : \"doggie\",  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ]} ]", List.class), HttpStatus.NOT_IMPLEMENTED);
                } catch (IOException e) {
                    log.error("Couldn't serialize response for content type application/json", e);
                    return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
                }
            }
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default PetApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see PetApi#findPetsByTags
     */
    default ResponseEntity<List<Pet>> findPetsByTags(List<String> tags) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
            if (getAcceptHeader().get().contains("application/xml")) {
                try {
                    return new ResponseEntity<>(getObjectMapper().get().readValue("<Pet>  <id>123456789</id>  <name>doggie</name>  <photoUrls>    <photoUrls>aeiou</photoUrls>  </photoUrls>  <tags>  </tags>  <status>aeiou</status></Pet>", List.class), HttpStatus.NOT_IMPLEMENTED);
                } catch (IOException e) {
                    log.error("Couldn't serialize response for content type application/xml", e);
                    return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
                }
            }
            if (getAcceptHeader().get().contains("application/json")) {
                try {
                    return new ResponseEntity<>(getObjectMapper().get().readValue("[ {  \"tags\" : [ {    \"id\" : 1,    \"name\" : \"name\"  }, {    \"id\" : 1,    \"name\" : \"name\"  } ],  \"id\" : 0,  \"category\" : {    \"id\" : 6,    \"name\" : \"name\"  },  \"status\" : \"available\",  \"name\" : \"doggie\",  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ]}, {  \"tags\" : [ {    \"id\" : 1,    \"name\" : \"name\"  }, {    \"id\" : 1,    \"name\" : \"name\"  } ],  \"id\" : 0,  \"category\" : {    \"id\" : 6,    \"name\" : \"name\"  },  \"status\" : \"available\",  \"name\" : \"doggie\",  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ]} ]", List.class), HttpStatus.NOT_IMPLEMENTED);
                } catch (IOException e) {
                    log.error("Couldn't serialize response for content type application/json", e);
                    return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
                }
            }
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default PetApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see PetApi#getPetById
     */
    default ResponseEntity<Pet> getPetById(Long petId) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
            if (getAcceptHeader().get().contains("application/xml")) {
                try {
                    return new ResponseEntity<>(getObjectMapper().get().readValue("<Pet>  <id>123456789</id>  <name>doggie</name>  <photoUrls>    <photoUrls>aeiou</photoUrls>  </photoUrls>  <tags>  </tags>  <status>aeiou</status></Pet>", Pet.class), HttpStatus.NOT_IMPLEMENTED);
                } catch (IOException e) {
                    log.error("Couldn't serialize response for content type application/xml", e);
                    return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
                }
            }
            if (getAcceptHeader().get().contains("application/json")) {
                try {
                    return new ResponseEntity<>(getObjectMapper().get().readValue("{  \"tags\" : [ {    \"id\" : 1,    \"name\" : \"name\"  }, {    \"id\" : 1,    \"name\" : \"name\"  } ],  \"id\" : 0,  \"category\" : {    \"id\" : 6,    \"name\" : \"name\"  },  \"status\" : \"available\",  \"name\" : \"doggie\",  \"photoUrls\" : [ \"photoUrls\", \"photoUrls\" ]}", Pet.class), HttpStatus.NOT_IMPLEMENTED);
                } catch (IOException e) {
                    log.error("Couldn't serialize response for content type application/json", e);
                    return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
                }
            }
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default PetApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see PetApi#updatePet
     */
    default ResponseEntity<Void> updatePet(Pet body) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default PetApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see PetApi#updatePetWithForm
     */
    default ResponseEntity<Void> updatePetWithForm(Long petId,
        String name,
        String status) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default PetApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see PetApi#uploadFile
     */
    default ResponseEntity<ModelApiResponse> uploadFile(Long petId,
        String additionalMetadata,
        MultipartFile file) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
            if (getAcceptHeader().get().contains("application/json")) {
                try {
                    return new ResponseEntity<>(getObjectMapper().get().readValue("{  \"message\" : \"message\",  \"code\" : 0,  \"type\" : \"type\"}", ModelApiResponse.class), HttpStatus.NOT_IMPLEMENTED);
                } catch (IOException e) {
                    log.error("Couldn't serialize response for content type application/json", e);
                    return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
                }
            }
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default PetApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

}

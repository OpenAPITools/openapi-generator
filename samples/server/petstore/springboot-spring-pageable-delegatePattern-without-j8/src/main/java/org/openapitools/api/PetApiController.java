package org.openapitools.api;

import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;
import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.*;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class PetApiController implements PetApi {

    private final PetApiDelegate delegate;

    public PetApiController(@org.springframework.beans.factory.annotation.Autowired(required = false) PetApiDelegate delegate) {
        this.delegate = delegate;
    }

    /**
     * POST /pet : Add a new pet to the store
     *
     * @param body Pet object that needs to be added to the store (required)
     * @return successful operation (status code 200)
     *         or Invalid input (status code 405)
     * @see PetApi#addPet
     */
    public ResponseEntity<Void> addPet(

@ApiParam(value = "Pet object that needs to be added to the store", required = true )   @Valid @RequestBody Pet body) {
        return delegate.addPet(body);
    }

    /**
     * DELETE /pet/{petId} : Deletes a pet
     *
     * @param petId Pet id to delete (required)
     * @param apiKey  (optional)
     * @return successful operation (status code 200)
     *         or Invalid pet value (status code 400)
     * @see PetApi#deletePet
     */
    public ResponseEntity<Void> deletePet(@ApiParam(value = "Pet id to delete", required = true) @PathVariable("petId") Long petId

,
@ApiParam(value = "") @RequestHeader(value = "api_key", required = false) String apiKey
) {
        return delegate.deletePet(petId, apiKey);
    }

    /**
     * GET /pet/findByStatus : Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     *
     * @param status Status values that need to be considered for filter (required)
     * @return successful operation (status code 200)
     *         or Invalid status value (status code 400)
     * @see PetApi#findPetsByStatus
     */
    public ResponseEntity<List<Pet>> findPetsByStatus(@NotNull @ApiParam(value = "Status values that need to be considered for filter", required = true, allowableValues = "available, pending, sold") @Valid @RequestParam(value = "status", required = true) List<String> status

, @springfox.documentation.annotations.ApiIgnore final Pageable pageable) {
        return delegate.findPetsByStatus(status, pageable);
    }

    /**
     * GET /pet/findByTags : Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     *
     * @param tags Tags to filter by (required)
     * @return successful operation (status code 200)
     *         or Invalid tag value (status code 400)
     * @deprecated
     * @see PetApi#findPetsByTags
     */
    public ResponseEntity<List<Pet>> findPetsByTags(@NotNull @ApiParam(value = "Tags to filter by", required = true) @Valid @RequestParam(value = "tags", required = true) List<String> tags

, @springfox.documentation.annotations.ApiIgnore final Pageable pageable) {
        return delegate.findPetsByTags(tags, pageable);
    }

    /**
     * GET /pet/{petId} : Find pet by ID
     * Returns a single pet
     *
     * @param petId ID of pet to return (required)
     * @return successful operation (status code 200)
     *         or Invalid ID supplied (status code 400)
     *         or Pet not found (status code 404)
     * @see PetApi#getPetById
     */
    public ResponseEntity<Pet> getPetById(@ApiParam(value = "ID of pet to return", required = true) @PathVariable("petId") Long petId

) {
        return delegate.getPetById(petId);
    }

    /**
     * PUT /pet : Update an existing pet
     *
     * @param body Pet object that needs to be added to the store (required)
     * @return successful operation (status code 200)
     *         or Invalid ID supplied (status code 400)
     *         or Pet not found (status code 404)
     *         or Validation exception (status code 405)
     * @see PetApi#updatePet
     */
    public ResponseEntity<Void> updatePet(

@ApiParam(value = "Pet object that needs to be added to the store", required = true )   @Valid @RequestBody Pet body) {
        return delegate.updatePet(body);
    }

    /**
     * POST /pet/{petId} : Updates a pet in the store with form data
     *
     * @param petId ID of pet that needs to be updated (required)
     * @param name Updated name of the pet (optional)
     * @param status Updated status of the pet (optional)
     * @return Invalid input (status code 405)
     * @see PetApi#updatePetWithForm
     */
    public ResponseEntity<Void> updatePetWithForm(@ApiParam(value = "ID of pet that needs to be updated", required = true) @PathVariable("petId") Long petId

,

@ApiParam(value = "Updated name of the pet") @Valid @RequestPart(value = "name", required = false) String name,

@ApiParam(value = "Updated status of the pet") @Valid @RequestPart(value = "status", required = false) String status) {
        return delegate.updatePetWithForm(petId, name, status);
    }

    /**
     * POST /pet/{petId}/uploadImage : uploads an image
     *
     * @param petId ID of pet to update (required)
     * @param additionalMetadata Additional data to pass to server (optional)
     * @param file file to upload (optional)
     * @return successful operation (status code 200)
     * @see PetApi#uploadFile
     */
    public ResponseEntity<ModelApiResponse> uploadFile(@ApiParam(value = "ID of pet to update", required = true) @PathVariable("petId") Long petId

,

@ApiParam(value = "Additional data to pass to server") @Valid @RequestPart(value = "additionalMetadata", required = false) String additionalMetadata,

@ApiParam(value = "file to upload") @Valid @RequestPart(value = "file", required = false) MultipartFile file) {
        return delegate.uploadFile(petId, additionalMetadata, file);
    }

}

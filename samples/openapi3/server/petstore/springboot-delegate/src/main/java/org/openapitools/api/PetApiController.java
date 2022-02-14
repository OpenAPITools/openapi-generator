package org.openapitools.api;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.Optional;
import javax.annotation.Generated;

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class PetApiController implements PetApi {

    private final PetApiDelegate delegate;

    public PetApiController(@Autowired(required = false) PetApiDelegate delegate) {
        this.delegate = Optional.ofNullable(delegate).orElse(new PetApiDelegate() {});
    }

    @Override
    public PetApiDelegate getDelegate() {
        return delegate;
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
        @Parameter(name = "body", description = "Pet object that needs to be added to the store", required = true) @Valid @RequestBody Pet body
    ) {
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
    public ResponseEntity<Void> deletePet(
        @Parameter(name = "petId", description = "Pet id to delete", required = true) @PathVariable("petId") Long petId,
        @Parameter(name = "api_key", description = "") @RequestHeader(value = "api_key", required = false) String apiKey
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
    public ResponseEntity<List<Pet>> findPetsByStatus(
        @NotNull @Parameter(name = "status", description = "Status values that need to be considered for filter", required = true) @Valid @RequestParam(value = "status", required = true) List<String> status
    ) {
        return delegate.findPetsByStatus(status);
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
    public ResponseEntity<Set<Pet>> findPetsByTags(
        @NotNull @Parameter(name = "tags", description = "Tags to filter by", required = true) @Valid @RequestParam(value = "tags", required = true) Set<String> tags
    ) {
        return delegate.findPetsByTags(tags);
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
    public ResponseEntity<Pet> getPetById(
        @Parameter(name = "petId", description = "ID of pet to return", required = true) @PathVariable("petId") Long petId
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
        @Parameter(name = "body", description = "Pet object that needs to be added to the store", required = true) @Valid @RequestBody Pet body
    ) {
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
    public ResponseEntity<Void> updatePetWithForm(
        @Parameter(name = "petId", description = "ID of pet that needs to be updated", required = true) @PathVariable("petId") Long petId,
        @Parameter(name = "name", description = "Updated name of the pet") @Valid @RequestPart(value = "name", required = false) String name,
        @Parameter(name = "status", description = "Updated status of the pet") @Valid @RequestPart(value = "status", required = false) String status
    ) {
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
    public ResponseEntity<ModelApiResponse> uploadFile(
        @Parameter(name = "petId", description = "ID of pet to update", required = true) @PathVariable("petId") Long petId,
        @Parameter(name = "additionalMetadata", description = "Additional data to pass to server") @Valid @RequestPart(value = "additionalMetadata", required = false) String additionalMetadata,
        @Parameter(name = "file", description = "file to upload") @RequestPart(value = "file", required = false) MultipartFile file
    ) {
        return delegate.uploadFile(petId, additionalMetadata, file);
    }

}

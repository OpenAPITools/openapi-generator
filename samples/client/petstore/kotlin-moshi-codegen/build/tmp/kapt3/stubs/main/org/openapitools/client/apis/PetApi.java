package org.openapitools.client.apis;

import java.lang.System;

@kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u0000>\n\u0002\u0018\u0002\n\u0002\u0018\u0002\n\u0000\n\u0002\u0010\u000e\n\u0002\b\u0002\n\u0002\u0010\u0002\n\u0000\n\u0002\u0018\u0002\n\u0002\b\u0002\n\u0002\u0010\t\n\u0002\b\u0002\n\u0002\u0010\u0011\n\u0002\b\t\n\u0002\u0018\u0002\n\u0002\b\u0002\n\u0002\u0018\u0002\n\u0000\u0018\u00002\u00020\u0001B\u000f\u0012\b\b\u0002\u0010\u0002\u001a\u00020\u0003\u00a2\u0006\u0002\u0010\u0004J\u000e\u0010\u0005\u001a\u00020\u00062\u0006\u0010\u0007\u001a\u00020\bJ\u0018\u0010\t\u001a\u00020\u00062\u0006\u0010\n\u001a\u00020\u000b2\b\u0010\f\u001a\u0004\u0018\u00010\u0003J\u001f\u0010\r\u001a\b\u0012\u0004\u0012\u00020\b0\u000e2\f\u0010\u000f\u001a\b\u0012\u0004\u0012\u00020\u00030\u000e\u00a2\u0006\u0002\u0010\u0010J\u001f\u0010\u0011\u001a\b\u0012\u0004\u0012\u00020\b0\u000e2\f\u0010\u0012\u001a\b\u0012\u0004\u0012\u00020\u00030\u000e\u00a2\u0006\u0002\u0010\u0010J\u000e\u0010\u0013\u001a\u00020\b2\u0006\u0010\n\u001a\u00020\u000bJ\u000e\u0010\u0014\u001a\u00020\u00062\u0006\u0010\u0007\u001a\u00020\bJ\"\u0010\u0015\u001a\u00020\u00062\u0006\u0010\n\u001a\u00020\u000b2\b\u0010\u0016\u001a\u0004\u0018\u00010\u00032\b\u0010\u000f\u001a\u0004\u0018\u00010\u0003J\"\u0010\u0017\u001a\u00020\u00182\u0006\u0010\n\u001a\u00020\u000b2\b\u0010\u0019\u001a\u0004\u0018\u00010\u00032\b\u0010\u001a\u001a\u0004\u0018\u00010\u001b\u00a8\u0006\u001c"}, d2 = {"Lorg/openapitools/client/apis/PetApi;", "Lorg/openapitools/client/infrastructure/ApiClient;", "basePath", "", "(Ljava/lang/String;)V", "addPet", "", "body", "Lorg/openapitools/client/models/Pet;", "deletePet", "petId", "", "apiKey", "findPetsByStatus", "", "status", "([Ljava/lang/String;)[Lorg/openapitools/client/models/Pet;", "findPetsByTags", "tags", "getPetById", "updatePet", "updatePetWithForm", "name", "uploadFile", "Lorg/openapitools/client/models/ApiResponse;", "additionalMetadata", "file", "Ljava/io/File;", "kotlin-petstore-moshi-codegen"})
public final class PetApi extends org.openapitools.client.infrastructure.ApiClient {
    
    /**
     * Add a new pet to the store
     *
     * @param body Pet object that needs to be added to the store 
     * @return void
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    public final void addPet(@org.jetbrains.annotations.NotNull()
    org.openapitools.client.models.Pet body) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
    }
    
    /**
     * Deletes a pet
     *
     * @param petId Pet id to delete 
     * @param apiKey  (optional)
     * @return void
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    public final void deletePet(long petId, @org.jetbrains.annotations.Nullable()
    java.lang.String apiKey) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
    }
    
    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * @param status Status values that need to be considered for filter 
     * @return kotlin.Array<Pet>
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @org.jetbrains.annotations.NotNull()
    @kotlin.Suppress(names = {"UNCHECKED_CAST"})
    public final org.openapitools.client.models.Pet[] findPetsByStatus(@org.jetbrains.annotations.NotNull()
    java.lang.String[] status) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
        return null;
    }
    
    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * @param tags Tags to filter by 
     * @return kotlin.Array<Pet>
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @org.jetbrains.annotations.NotNull()
    @kotlin.Suppress(names = {"UNCHECKED_CAST"})
    public final org.openapitools.client.models.Pet[] findPetsByTags(@org.jetbrains.annotations.NotNull()
    java.lang.String[] tags) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
        return null;
    }
    
    /**
     * Find pet by ID
     * Returns a single pet
     * @param petId ID of pet to return 
     * @return Pet
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @org.jetbrains.annotations.NotNull()
    @kotlin.Suppress(names = {"UNCHECKED_CAST"})
    public final org.openapitools.client.models.Pet getPetById(long petId) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
        return null;
    }
    
    /**
     * Update an existing pet
     *
     * @param body Pet object that needs to be added to the store 
     * @return void
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    public final void updatePet(@org.jetbrains.annotations.NotNull()
    org.openapitools.client.models.Pet body) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
    }
    
    /**
     * Updates a pet in the store with form data
     *
     * @param petId ID of pet that needs to be updated 
     * @param name Updated name of the pet (optional)
     * @param status Updated status of the pet (optional)
     * @return void
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    public final void updatePetWithForm(long petId, @org.jetbrains.annotations.Nullable()
    java.lang.String name, @org.jetbrains.annotations.Nullable()
    java.lang.String status) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
    }
    
    /**
     * uploads an image
     *
     * @param petId ID of pet to update 
     * @param additionalMetadata Additional data to pass to server (optional)
     * @param file file to upload (optional)
     * @return ApiResponse
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @org.jetbrains.annotations.NotNull()
    @kotlin.Suppress(names = {"UNCHECKED_CAST"})
    public final org.openapitools.client.models.ApiResponse uploadFile(long petId, @org.jetbrains.annotations.Nullable()
    java.lang.String additionalMetadata, @org.jetbrains.annotations.Nullable()
    java.io.File file) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
        return null;
    }
    
    public PetApi(@org.jetbrains.annotations.NotNull()
    java.lang.String basePath) {
        super(null);
    }
    
    public PetApi() {
        super(null);
    }
}
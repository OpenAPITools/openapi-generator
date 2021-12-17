// TODO: better import syntax?
import { BaseAPIRequestFactory, RequiredError } from './baseapi';
import {Configuration} from '../configuration';
import { RequestContext, HttpMethod, ResponseContext, HttpFile} from '../http/http';
import {ObjectSerializer} from '../models/ObjectSerializer';
import {ApiException} from './exception';
import {canConsumeForm, isCodeInRange} from '../util';


import { ApiResponse } from '../models/ApiResponse';
import { Pet } from '../models/Pet';

/**
 * no description
 */
export class PetApiRequestFactory extends BaseAPIRequestFactory {

    /**
     * Add a new pet to the store
     * @param pet Pet object that needs to be added to the store
     */
    public async addPet(pet: Pet, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'pet' is not null or undefined
        if (pet === null || pet === undefined) {
            throw new RequiredError("PetApi", "addPet", "pet");
        }


        // Path Params
        const localVarPath = '/pet';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "application/json",
        
            "application/xml"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(pet, "Pet", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        let authMethod = null;
        // Apply auth methods
        authMethod = _config.authMethods["petstore_auth"]
        if (authMethod) {
            await authMethod.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Deletes a pet
     * @param petId Pet id to delete
     * @param apiKey 
     */
    public async deletePet(petId: number, apiKey?: string, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'petId' is not null or undefined
        if (petId === null || petId === undefined) {
            throw new RequiredError("PetApi", "deletePet", "petId");
        }



        // Path Params
        const localVarPath = '/pet/{petId}'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.DELETE);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Header Params
        requestContext.setHeaderParam("api_key", ObjectSerializer.serialize(apiKey, "string", ""));


        let authMethod = null;
        // Apply auth methods
        authMethod = _config.authMethods["petstore_auth"]
        if (authMethod) {
            await authMethod.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Multiple status values can be provided with comma separated strings
     * Finds Pets by status
     * @param status Status values that need to be considered for filter
     */
    public async findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'status' is not null or undefined
        if (status === null || status === undefined) {
            throw new RequiredError("PetApi", "findPetsByStatus", "status");
        }


        // Path Params
        const localVarPath = '/pet/findByStatus';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
        if (status !== undefined) {
            requestContext.setQueryParam("status", ObjectSerializer.serialize(status, "Array<'available' | 'pending' | 'sold'>", ""));
        }


        let authMethod = null;
        // Apply auth methods
        authMethod = _config.authMethods["petstore_auth"]
        if (authMethod) {
            await authMethod.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Finds Pets by tags
     * @param tags Tags to filter by
     */
    public async findPetsByTags(tags: Array<string>, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'tags' is not null or undefined
        if (tags === null || tags === undefined) {
            throw new RequiredError("PetApi", "findPetsByTags", "tags");
        }


        // Path Params
        const localVarPath = '/pet/findByTags';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
        if (tags !== undefined) {
            requestContext.setQueryParam("tags", ObjectSerializer.serialize(tags, "Array<string>", ""));
        }


        let authMethod = null;
        // Apply auth methods
        authMethod = _config.authMethods["petstore_auth"]
        if (authMethod) {
            await authMethod.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Returns a single pet
     * Find pet by ID
     * @param petId ID of pet to return
     */
    public async getPetById(petId: number, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'petId' is not null or undefined
        if (petId === null || petId === undefined) {
            throw new RequiredError("PetApi", "getPetById", "petId");
        }


        // Path Params
        const localVarPath = '/pet/{petId}'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        let authMethod = null;
        // Apply auth methods
        authMethod = _config.authMethods["api_key"]
        if (authMethod) {
            await authMethod.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Update an existing pet
     * @param pet Pet object that needs to be added to the store
     */
    public async updatePet(pet: Pet, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'pet' is not null or undefined
        if (pet === null || pet === undefined) {
            throw new RequiredError("PetApi", "updatePet", "pet");
        }


        // Path Params
        const localVarPath = '/pet';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.PUT);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "application/json",
        
            "application/xml"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(pet, "Pet", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        let authMethod = null;
        // Apply auth methods
        authMethod = _config.authMethods["petstore_auth"]
        if (authMethod) {
            await authMethod.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Updates a pet in the store with form data
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet
     * @param status Updated status of the pet
     */
    public async updatePetWithForm(petId: number, name?: string, status?: string, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'petId' is not null or undefined
        if (petId === null || petId === undefined) {
            throw new RequiredError("PetApi", "updatePetWithForm", "petId");
        }




        // Path Params
        const localVarPath = '/pet/{petId}'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Form Params
        const useForm = canConsumeForm([
            'application/x-www-form-urlencoded',
        ]);

        let localVarFormParams
        if (useForm) {
            localVarFormParams = new FormData();
        } else {
            localVarFormParams = new URLSearchParams();
        }

        if (name !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('name', name as any);
        }
        if (status !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('status', status as any);
        }

        requestContext.setBody(localVarFormParams);

        if(!useForm) {
            const contentType = ObjectSerializer.getPreferredMediaType([
                "application/x-www-form-urlencoded"
            ]);
            requestContext.setHeaderParam("Content-Type", contentType);
        }

        let authMethod = null;
        // Apply auth methods
        authMethod = _config.authMethods["petstore_auth"]
        if (authMethod) {
            await authMethod.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * uploads an image
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param file file to upload
     */
    public async uploadFile(petId: number, additionalMetadata?: string, file?: HttpFile, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'petId' is not null or undefined
        if (petId === null || petId === undefined) {
            throw new RequiredError("PetApi", "uploadFile", "petId");
        }




        // Path Params
        const localVarPath = '/pet/{petId}/uploadImage'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Form Params
        const useForm = canConsumeForm([
            'multipart/form-data',
        ]);

        let localVarFormParams
        if (useForm) {
            localVarFormParams = new FormData();
        } else {
            localVarFormParams = new URLSearchParams();
        }

        if (additionalMetadata !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('additionalMetadata', additionalMetadata as any);
        }
        if (file !== undefined) {
             // TODO: replace .append with .set
             if (localVarFormParams instanceof FormData) {
                 localVarFormParams.append('file', file, file.name);
             }
        }

        requestContext.setBody(localVarFormParams);

        if(!useForm) {
            const contentType = ObjectSerializer.getPreferredMediaType([
                "multipart/form-data"
            ]);
            requestContext.setHeaderParam("Content-Type", contentType);
        }

        let authMethod = null;
        // Apply auth methods
        authMethod = _config.authMethods["petstore_auth"]
        if (authMethod) {
            await authMethod.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

}

export class PetApiResponseProcessor {

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to addPet
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async addPet(response: ResponseContext): Promise<Pet > {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: Pet = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "Pet", ""
            ) as Pet;
            return body;
        }
        if (isCodeInRange("405", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Invalid input", undefined, response.headers);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: Pet = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "Pet", ""
            ) as Pet;
            return body;
        }

        throw new ApiException<string | Blob | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to deletePet
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async deletePet(response: ResponseContext): Promise< void> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Invalid pet value", undefined, response.headers);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            return;
        }

        throw new ApiException<string | Blob | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to findPetsByStatus
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async findPetsByStatus(response: ResponseContext): Promise<Array<Pet> > {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: Array<Pet> = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "Array<Pet>", ""
            ) as Array<Pet>;
            return body;
        }
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Invalid status value", undefined, response.headers);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: Array<Pet> = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "Array<Pet>", ""
            ) as Array<Pet>;
            return body;
        }

        throw new ApiException<string | Blob | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to findPetsByTags
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async findPetsByTags(response: ResponseContext): Promise<Array<Pet> > {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: Array<Pet> = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "Array<Pet>", ""
            ) as Array<Pet>;
            return body;
        }
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Invalid tag value", undefined, response.headers);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: Array<Pet> = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "Array<Pet>", ""
            ) as Array<Pet>;
            return body;
        }

        throw new ApiException<string | Blob | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to getPetById
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async getPetById(response: ResponseContext): Promise<Pet > {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: Pet = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "Pet", ""
            ) as Pet;
            return body;
        }
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Invalid ID supplied", undefined, response.headers);
        }
        if (isCodeInRange("404", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Pet not found", undefined, response.headers);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: Pet = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "Pet", ""
            ) as Pet;
            return body;
        }

        throw new ApiException<string | Blob | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to updatePet
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async updatePet(response: ResponseContext): Promise<Pet > {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: Pet = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "Pet", ""
            ) as Pet;
            return body;
        }
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Invalid ID supplied", undefined, response.headers);
        }
        if (isCodeInRange("404", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Pet not found", undefined, response.headers);
        }
        if (isCodeInRange("405", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Validation exception", undefined, response.headers);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: Pet = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "Pet", ""
            ) as Pet;
            return body;
        }

        throw new ApiException<string | Blob | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to updatePetWithForm
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async updatePetWithForm(response: ResponseContext): Promise< void> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("405", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Invalid input", undefined, response.headers);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            return;
        }

        throw new ApiException<string | Blob | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to uploadFile
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async uploadFile(response: ResponseContext): Promise<ApiResponse > {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: ApiResponse = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "ApiResponse", ""
            ) as ApiResponse;
            return body;
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: ApiResponse = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "ApiResponse", ""
            ) as ApiResponse;
            return body;
        }

        throw new ApiException<string | Blob | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

}

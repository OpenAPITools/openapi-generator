// TODO: better import syntax?
import { BaseApiRequestFactory } from './baseapi';
import { RequestContext } from '../http/http';
import { ApiResponse } from '../models/ApiResponse';
import { Pet } from '../models/Pet';




/**
 * PetApi - interface
 * @export
 * @interface PetApi
 */
export class PetApiRequestFactory {

    /**
     * 
     * @summary Add a new pet to the store
     * @param {Pet} pet Pet object that needs to be added to the store
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    public addPet(pet: Pet, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * 
     * @summary Deletes a pet
     * @param {number} petId Pet id to delete
     * @param {string} [apiKey] 
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    public deletePet(petId: number, apiKey?: string, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * Multiple status values can be provided with comma separated strings
     * @summary Finds Pets by status
     * @param {Array<'available' | 'pending' | 'sold'>} status Status values that need to be considered for filter
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    public findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * @summary Finds Pets by tags
     * @param {Array<string>} tags Tags to filter by
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    public findPetsByTags(tags: Array<string>, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * Returns a single pet
     * @summary Find pet by ID
     * @param {number} petId ID of pet to return
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    public getPetById(petId: number, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * 
     * @summary Update an existing pet
     * @param {Pet} pet Pet object that needs to be added to the store
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    public updatePet(pet: Pet, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * 
     * @summary Updates a pet in the store with form data
     * @param {number} petId ID of pet that needs to be updated
     * @param {string} [name] Updated name of the pet
     * @param {string} [status] Updated status of the pet
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    public updatePetWithForm(petId: number, name?: string, status?: string, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * 
     * @summary uploads an image
     * @param {number} petId ID of pet to update
     * @param {string} [additionalMetadata] Additional data to pass to server
     * @param {any} [file] file to upload
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    public uploadFile(petId: number, additionalMetadata?: string, file?: any, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
}

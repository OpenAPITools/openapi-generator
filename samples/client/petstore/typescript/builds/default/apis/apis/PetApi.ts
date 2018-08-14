// TODO: better import syntax?

import { ApiResponse } from '../';
import { Pet } from '../';
/**
 * PetApi - interface
 * @export
 * @interface PetApi
 */
export interface PetApiInterface {
    /**
     * 
     * @summary Add a new pet to the store
     * @param {Pet} pet Pet object that needs to be added to the store
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    addPet(pet: Pet, options?: any): Promise<{}>;

    /**
     * 
     * @summary Deletes a pet
     * @param {number} petId Pet id to delete
     * @param {string} [apiKey] 
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    deletePet(petId: number, apiKey?: string, options?: any): Promise<{}>;

    /**
     * Multiple status values can be provided with comma separated strings
     * @summary Finds Pets by status
     * @param {Array<'available' | 'pending' | 'sold'>} status Status values that need to be considered for filter
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: any): Promise<Array<Pet>>;

    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * @summary Finds Pets by tags
     * @param {Array<string>} tags Tags to filter by
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    findPetsByTags(tags: Array<string>, options?: any): Promise<Array<Pet>>;

    /**
     * Returns a single pet
     * @summary Find pet by ID
     * @param {number} petId ID of pet to return
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    getPetById(petId: number, options?: any): Promise<Pet>;

    /**
     * 
     * @summary Update an existing pet
     * @param {Pet} pet Pet object that needs to be added to the store
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof PetApiInterface
     */
    updatePet(pet: Pet, options?: any): Promise<{}>;

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
    updatePetWithForm(petId: number, name?: string, status?: string, options?: any): Promise<{}>;

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
    uploadFile(petId: number, additionalMetadata?: string, file?: any, options?: any): Promise<ApiResponse>;

}

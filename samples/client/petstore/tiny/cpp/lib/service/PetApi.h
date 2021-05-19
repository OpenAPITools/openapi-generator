#ifndef TINY_CPP_CLIENT_PetApi_H_
#define TINY_CPP_CLIENT_PetApi_H_


#include "Response.h"
#include "Arduino.h"
#include "AbstractService.h"
#include "Helpers.h"
#include <list>

#include "ApiResponse.h"
#include "Pet.h"

namespace Tiny {

/**
 *  Class 
 * Generated with openapi::tiny-cpp-client
 */

class PetApi : public AbstractService {
public:
    PetApi() = default;

    virtual ~PetApi() = default;

    /**
    * Add a new pet to the store.
    *
    * 
    * \param pet Pet object that needs to be added to the store *Required*
    */
    Response<
                Pet
        >
    addPet(
            
            Pet pet
            
    );
    /**
    * Deletes a pet.
    *
    * 
    * \param petId Pet id to delete *Required*
    * \param apiKey 
    */
    Response<
            String
        >
    deletePet(
            
            long petId
            , 
            
            std::string apiKey
            
    );
    /**
    * Finds Pets by status.
    *
    * Multiple status values can be provided with comma separated strings
    * \param status Status values that need to be considered for filter *Required*
    */
    Response<
                    std::list<Pet>
        >
    findPetsByStatus(
            std::list<std::string> status
            
            
    );
    /**
    * Finds Pets by tags.
    *
    * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    * \param tags Tags to filter by *Required*
    */
    Response<
                    std::list<Pet>
        >
    findPetsByTags(
            std::list<std::string> tags
            
            
    );
    /**
    * Find pet by ID.
    *
    * Returns a single pet
    * \param petId ID of pet to return *Required*
    */
    Response<
                Pet
        >
    getPetById(
            
            long petId
            
    );
    /**
    * Update an existing pet.
    *
    * 
    * \param pet Pet object that needs to be added to the store *Required*
    */
    Response<
                Pet
        >
    updatePet(
            
            Pet pet
            
    );
    /**
    * Updates a pet in the store with form data.
    *
    * 
    * \param petId ID of pet that needs to be updated *Required*
    * \param name Updated name of the pet
    * \param status Updated status of the pet
    */
    Response<
            String
        >
    updatePetWithForm(
            
            long petId
            , 
            
            std::string name
            , 
            
            std::string status
            
    );
    /**
    * uploads an image.
    *
    * 
    * \param petId ID of pet to update *Required*
    * \param additionalMetadata Additional data to pass to server
    * \param file file to upload
    */
    Response<
                ApiResponse
        >
    uploadFile(
            
            long petId
            , 
            
            std::string additionalMetadata
            , 
            
            std::string file
            
    );
}; 

} 

#endif /* TINY_CPP_CLIENT_PetApi_H_ */
#include <stdlib.h>
#include <stdio.h>
#include "apiClient.h"
#include "cJSON.h"
#include "api_response.h"
#include "pet.h"


// Add a new pet to the store
//
void PetAPI_addPet(apiClient_t *apiClient, pet_t *pet);



// Deletes a pet
//
void PetAPI_deletePet(apiClient_t *apiClient, long petId, char *api_key);



// Finds Pets by status
//
// Multiple status values can be provided with comma separated strings
//
list_t *PetAPI_findPetsByStatus(apiClient_t *apiClient, list_t *status);



// Finds Pets by tags
//
// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
//
list_t *PetAPI_findPetsByTags(apiClient_t *apiClient, list_t *tags);



// Find pet by ID
//
// Returns a single pet
//
pet_t *PetAPI_getPetById(apiClient_t *apiClient, long petId);



// Update an existing pet
//
void PetAPI_updatePet(apiClient_t *apiClient, pet_t *pet);



// Updates a pet in the store with form data
//
void PetAPI_updatePetWithForm(apiClient_t *apiClient, long petId, char *name,
                              char *status);



// uploads an image
//
api_response_t *PetAPI_uploadFile(apiClient_t *apiClient, long petId,
                                  char *additionalMetadata, FILE *file);

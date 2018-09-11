#include <stdlib.h>
#include <stdio.h>
#include "apiClient.h"
#include "cJSON.h"
#include "models.api_response.h" // TODO will fix the import later
#include "models.pet.h" // TODO will fix the import later

// Add a new pet to the store
//
void *PetAPI_addPet(apiClient_t *apiClient, pet Pet) 

// Deletes a pet
//
void *PetAPI_deletePet(apiClient_t *apiClient, long PetId, char ApiKey) 

// Finds Pets by status
//
// Multiple status values can be provided with comma separated strings
//
list_t *PetAPI_findPetsByStatus(apiClient_t *apiClient, list Status) 

// Finds Pets by tags
//
// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
//
list_t *PetAPI_findPetsByTags(apiClient_t *apiClient, list Tags) 

// Find pet by ID
//
// Returns a single pet
//
pet_t *PetAPI_getPetById(apiClient_t *apiClient, long PetId) 

// Update an existing pet
//
void *PetAPI_updatePet(apiClient_t *apiClient, pet Pet) 

// Updates a pet in the store with form data
//
void *PetAPI_updatePetWithForm(apiClient_t *apiClient, long PetId, char Name, char Status) 

// uploads an image
//
api_response_t *PetAPI_uploadFile(apiClient_t *apiClient, long PetId, char AdditionalMetadata, FILE File) 




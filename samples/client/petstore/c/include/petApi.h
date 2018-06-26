#ifndef INCLUDE_PET_API_H
#define INCLUDE_PET_API_H

#include "pet.h"
#include "apiClient.h"

pet_t* petApi_getPetById(apiClient_t *apiClient, long petId);
void *petApi_addPet(apiClient_t *apiClient, pet_t *pet);

#endif // INCLUDE_PET_API_H

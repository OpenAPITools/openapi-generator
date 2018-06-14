#ifndef INCLUDE_PET_API_H
#define INCLUDE_PET_API_H

#include "pet.h"

pet_t* petApi_getPetById(long petId);
void *petApi_addPet(pet_t *pet);

#endif // INCLUDE_PET_API_H

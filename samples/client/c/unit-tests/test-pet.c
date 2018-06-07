#include "pet.h"

int main() {
	pet_t *pet = pet_create();
	pet_free(pet);
}
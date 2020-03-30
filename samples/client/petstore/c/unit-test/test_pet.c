#ifndef pet_TEST
#define pet_TEST

// the following is to include only the main from the first c file
#ifndef TEST_MAIN
#define TEST_MAIN
#define pet_MAIN
#endif // TEST_MAIN

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "../external/cJSON.h"

#include "../model/pet.h"
pet_t* instantiate_pet(int include_optional);

#include "test_category.c"


pet_t* instantiate_pet(int include_optional) {
  pet_t* pet = NULL;
  if (include_optional) {
    pet = pet_create(
      56,
       // false, not to have infinite recursion
      instantiate_category(0),
      "doggie",
      list_create(),
      list_create(),
      openapi_petstore_pet_STATUS_available
    );
  } else {
    pet = pet_create(
      56,
      NULL,
      "doggie",
      list_create(),
      list_create(),
      openapi_petstore_pet_STATUS_available
    );
  }

  return pet;
}


#ifdef pet_MAIN

void test_pet(int include_optional) {
    pet_t* pet_1 = instantiate_pet(include_optional);

	cJSON* jsonpet_1 = pet_convertToJSON(pet_1);
	printf("pet :\n%s\n", cJSON_Print(jsonpet_1));
	pet_t* pet_2 = pet_parseFromJSON(jsonpet_1);
	cJSON* jsonpet_2 = pet_convertToJSON(pet_2);
	printf("repeating pet:\n%s\n", cJSON_Print(jsonpet_2));
}

int main() {
  test_pet(1);
  test_pet(0);

  printf("Hello world \n");
  return 0;
}

#endif // pet_MAIN
#endif // pet_TEST

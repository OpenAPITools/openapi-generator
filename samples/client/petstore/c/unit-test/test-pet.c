#ifndef _TEST
#define _TEST

// the following is to include only the main from the first c file
#ifndef TEST_MAIN
#define TEST_MAIN
#define _MAIN
#endif // TEST_MAIN

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "../external/cJSON.h"

// this one in import:
#include "../model/category.h"
// this one in import:
#include "../model/tag.h"

#include "../model/pet.h"
pet_t* instantiate_pet(int include_optional);


pet_t* instantiate_pet(int include_optional) {
  pet_t* pet = pet_create(
56
//primitive
, // id


    
	// modello normale category_t *
0, // category
"doggie"
//primitive
, // name
list_create()
//primitive
  //list list_t *
  
, // photo_urls
list_create()

  //list list_t *
  
, // tags
openapi_petstore_pet_STATUS_available
//primitive
 // status
  );
return pet;
}


#ifdef _MAIN

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

#endif // _MAIN
#endif // _TEST

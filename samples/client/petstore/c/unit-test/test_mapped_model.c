#ifndef mapped_model_TEST
#define mapped_model_TEST

// the following is to include only the main from the first c file
#ifndef TEST_MAIN
#define TEST_MAIN
#define mapped_model_MAIN
#endif // TEST_MAIN

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "../external/cJSON.h"

#include "../model/mapped_model.h"
MappedModel_t* instantiate_MappedModel(int include_optional);



MappedModel_t* instantiate_MappedModel(int include_optional) {
  MappedModel_t* MappedModel = NULL;
  if (include_optional) {
    MappedModel = MappedModel_create(
      56
    );
  } else {
    MappedModel = MappedModel_create(
      56
    );
  }

  return MappedModel;
}


#ifdef mapped_model_MAIN

void test_MappedModel(int include_optional) {
    MappedModel_t* MappedModel_1 = instantiate_MappedModel(include_optional);

	cJSON* jsonMappedModel_1 = MappedModel_convertToJSON(MappedModel_1);
	printf("MappedModel :\n%s\n", cJSON_Print(jsonMappedModel_1));
	MappedModel_t* MappedModel_2 = MappedModel_parseFromJSON(jsonMappedModel_1);
	cJSON* jsonMappedModel_2 = MappedModel_convertToJSON(MappedModel_2);
	printf("repeating MappedModel:\n%s\n", cJSON_Print(jsonMappedModel_2));
}

int main() {
  test_MappedModel(1);
  test_MappedModel(0);

  printf("Hello world \n");
  return 0;
}

#endif // mapped_model_MAIN
#endif // mapped_model_TEST

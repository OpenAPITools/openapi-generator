#ifndef model_with_set_properties_TEST
#define model_with_set_properties_TEST

// the following is to include only the main from the first c file
#ifndef TEST_MAIN
#define TEST_MAIN
#define model_with_set_properties_MAIN
#endif // TEST_MAIN

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "../external/cJSON.h"

#include "../model/model_with_set_properties.h"
model_with_set_properties_t* instantiate_model_with_set_properties(int include_optional);



model_with_set_properties_t* instantiate_model_with_set_properties(int include_optional) {
  model_with_set_properties_t* model_with_set_properties = NULL;
  if (include_optional) {
    model_with_set_properties = model_with_set_properties_create(
      list_createList(),
      list_createList()
    );
  } else {
    model_with_set_properties = model_with_set_properties_create(
      list_createList(),
      list_createList()
    );
  }

  return model_with_set_properties;
}


#ifdef model_with_set_properties_MAIN

void test_model_with_set_properties(int include_optional) {
    model_with_set_properties_t* model_with_set_properties_1 = instantiate_model_with_set_properties(include_optional);

	cJSON* jsonmodel_with_set_properties_1 = model_with_set_properties_convertToJSON(model_with_set_properties_1);
	printf("model_with_set_properties :\n%s\n", cJSON_PrintUnformatted(jsonmodel_with_set_properties_1));
	model_with_set_properties_t* model_with_set_properties_2 = model_with_set_properties_parseFromJSON(jsonmodel_with_set_properties_1);
	cJSON* jsonmodel_with_set_properties_2 = model_with_set_properties_convertToJSON(model_with_set_properties_2);
	printf("repeating model_with_set_properties:\n%s\n", cJSON_PrintUnformatted(jsonmodel_with_set_properties_2));
}

int main() {
  test_model_with_set_properties(1);
  test_model_with_set_properties(0);

  printf("Hello world \n");
  return 0;
}

#endif // model_with_set_properties_MAIN
#endif // model_with_set_properties_TEST

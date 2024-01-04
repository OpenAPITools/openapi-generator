#ifndef model_with_set_propertes_TEST
#define model_with_set_propertes_TEST

// the following is to include only the main from the first c file
#ifndef TEST_MAIN
#define TEST_MAIN
#define model_with_set_propertes_MAIN
#endif // TEST_MAIN

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "../external/cJSON.h"

#include "../model/model_with_set_propertes.h"
model_with_set_propertes_t* instantiate_model_with_set_propertes(int include_optional);



model_with_set_propertes_t* instantiate_model_with_set_propertes(int include_optional) {
  model_with_set_propertes_t* model_with_set_propertes = NULL;
  if (include_optional) {
    model_with_set_propertes = model_with_set_propertes_create(
      list_createList(),
      list_createList()
    );
  } else {
    model_with_set_propertes = model_with_set_propertes_create(
      list_createList(),
      list_createList()
    );
  }

  return model_with_set_propertes;
}


#ifdef model_with_set_propertes_MAIN

void test_model_with_set_propertes(int include_optional) {
    model_with_set_propertes_t* model_with_set_propertes_1 = instantiate_model_with_set_propertes(include_optional);

	cJSON* jsonmodel_with_set_propertes_1 = model_with_set_propertes_convertToJSON(model_with_set_propertes_1);
	printf("model_with_set_propertes :\n%s\n", cJSON_Print(jsonmodel_with_set_propertes_1));
	model_with_set_propertes_t* model_with_set_propertes_2 = model_with_set_propertes_parseFromJSON(jsonmodel_with_set_propertes_1);
	cJSON* jsonmodel_with_set_propertes_2 = model_with_set_propertes_convertToJSON(model_with_set_propertes_2);
	printf("repeating model_with_set_propertes:\n%s\n", cJSON_Print(jsonmodel_with_set_propertes_2));
}

int main() {
  test_model_with_set_propertes(1);
  test_model_with_set_propertes(0);

  printf("Hello world \n");
  return 0;
}

#endif // model_with_set_propertes_MAIN
#endif // model_with_set_propertes_TEST

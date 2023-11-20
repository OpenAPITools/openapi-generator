#ifndef another_model_TEST
#define another_model_TEST

// the following is to include only the main from the first c file
#ifndef TEST_MAIN
#define TEST_MAIN
#define another_model_MAIN
#endif // TEST_MAIN

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "../external/cJSON.h"

#include "../model/another_model.h"
another_model_t* instantiate_another_model(int include_optional);



another_model_t* instantiate_another_model(int include_optional) {
  another_model_t* another_model = NULL;
  if (include_optional) {
    another_model = another_model_create(
      56,
      "0"
    );
  } else {
    another_model = another_model_create(
      56,
      "0"
    );
  }

  return another_model;
}


#ifdef another_model_MAIN

void test_another_model(int include_optional) {
    another_model_t* another_model_1 = instantiate_another_model(include_optional);

	cJSON* jsonanother_model_1 = another_model_convertToJSON(another_model_1);
	printf("another_model :\n%s\n", cJSON_Print(jsonanother_model_1));
	another_model_t* another_model_2 = another_model_parseFromJSON(jsonanother_model_1);
	cJSON* jsonanother_model_2 = another_model_convertToJSON(another_model_2);
	printf("repeating another_model:\n%s\n", cJSON_Print(jsonanother_model_2));
}

int main() {
  test_another_model(1);
  test_another_model(0);

  printf("Hello world \n");
  return 0;
}

#endif // another_model_MAIN
#endif // another_model_TEST

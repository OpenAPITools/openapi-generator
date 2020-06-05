#ifndef category_TEST
#define category_TEST

// the following is to include only the main from the first c file
#ifndef TEST_MAIN
#define TEST_MAIN
#define category_MAIN
#endif // TEST_MAIN

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "../external/cJSON.h"

#include "../model/category.h"
category_t* instantiate_category(int include_optional);



category_t* instantiate_category(int include_optional) {
  category_t* category = NULL;
  if (include_optional) {
    category = category_create(
      56,
      "0"
    );
  } else {
    category = category_create(
      56,
      "0"
    );
  }

  return category;
}


#ifdef category_MAIN

void test_category(int include_optional) {
    category_t* category_1 = instantiate_category(include_optional);

	cJSON* jsoncategory_1 = category_convertToJSON(category_1);
	printf("category :\n%s\n", cJSON_Print(jsoncategory_1));
	category_t* category_2 = category_parseFromJSON(jsoncategory_1);
	cJSON* jsoncategory_2 = category_convertToJSON(category_2);
	printf("repeating category:\n%s\n", cJSON_Print(jsoncategory_2));
}

int main() {
  test_category(1);
  test_category(0);

  printf("Hello world \n");
  return 0;
}

#endif // category_MAIN
#endif // category_TEST

#ifndef tag_TEST
#define tag_TEST

// the following is to include only the main from the first c file
#ifndef TEST_MAIN
#define TEST_MAIN
#define tag_MAIN
#endif // TEST_MAIN

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "../external/cJSON.h"

#include "../model/tag.h"
tag_t* instantiate_tag(int include_optional);



tag_t* instantiate_tag(int include_optional) {
  tag_t* tag = NULL;
  if (include_optional) {
    tag = tag_create(
      56,
      "0"
    );
  } else {
    tag = tag_create(
      56,
      "0"
    );
  }

  return tag;
}


#ifdef tag_MAIN

void test_tag(int include_optional) {
    tag_t* tag_1 = instantiate_tag(include_optional);

	cJSON* jsontag_1 = tag_convertToJSON(tag_1);
	printf("tag :\n%s\n", cJSON_Print(jsontag_1));
	tag_t* tag_2 = tag_parseFromJSON(jsontag_1);
	cJSON* jsontag_2 = tag_convertToJSON(tag_2);
	printf("repeating tag:\n%s\n", cJSON_Print(jsontag_2));
}

int main() {
  test_tag(1);
  test_tag(0);

  printf("Hello world \n");
  return 0;
}

#endif // tag_MAIN
#endif // tag_TEST

#ifndef flags_TEST
#define flags_TEST

// the following is to include only the main from the first c file
#ifndef TEST_MAIN
#define TEST_MAIN
#define flags_MAIN
#endif // TEST_MAIN

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "../external/cJSON.h"

#include "../model/flags.h"
flags_t* instantiate_flags(int include_optional);



flags_t* instantiate_flags(int include_optional) {
  flags_t* flags = NULL;
  if (include_optional) {
    flags = flags_create(
      list_createList()
    );
  } else {
    flags = flags_create(
      list_createList()
    );
  }

  return flags;
}


#ifdef flags_MAIN

void test_flags(int include_optional) {
    flags_t* flags_1 = instantiate_flags(include_optional);

	cJSON* jsonflags_1 = flags_convertToJSON(flags_1);
	printf("flags :\n%s\n", cJSON_Print(jsonflags_1));
	flags_t* flags_2 = flags_parseFromJSON(jsonflags_1);
	cJSON* jsonflags_2 = flags_convertToJSON(flags_2);
	printf("repeating flags:\n%s\n", cJSON_Print(jsonflags_2));
}

int main() {
  test_flags(1);
  test_flags(0);

  printf("Hello world \n");
  return 0;
}

#endif // flags_MAIN
#endif // flags_TEST

#ifndef preference_TEST
#define preference_TEST

// the following is to include only the main from the first c file
#ifndef TEST_MAIN
#define TEST_MAIN
#define preference_MAIN
#endif // TEST_MAIN

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "../external/cJSON.h"

#include "../model/preference.h"
preference_t* instantiate_preference(int include_optional);



preference_t* instantiate_preference(int include_optional) {
  preference_t* preference = NULL;
  if (include_optional) {
    preference = preference_create(
    );
  } else {
    preference = preference_create(
    );
  }

  return preference;
}


#ifdef preference_MAIN

void test_preference(int include_optional) {
    preference_t* preference_1 = instantiate_preference(include_optional);

	cJSON* jsonpreference_1 = preference_convertToJSON(preference_1);
	printf("preference :\n%s\n", cJSON_PrintUnformatted(jsonpreference_1));
	preference_t* preference_2 = preference_parseFromJSON(jsonpreference_1);
	cJSON* jsonpreference_2 = preference_convertToJSON(preference_2);
	printf("repeating preference:\n%s\n", cJSON_PrintUnformatted(jsonpreference_2));
}

int main() {
  test_preference(1);
  test_preference(0);

  printf("Hello world \n");
  return 0;
}

#endif // preference_MAIN
#endif // preference_TEST

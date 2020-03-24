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


#include "../model/user.h"
user_t* instantiate_user(int include_optional);


user_t* instantiate_user(int include_optional) {
  user_t* user = user_create(
56
//primitive
, // id
"0"
//primitive
, // username
"0"
//primitive
, // first_name
"0"
//primitive
, // last_name
"0"
//primitive
, // email
"0"
//primitive
, // password
"0"
//primitive
, // phone
56
//primitive
 // user_status
  );
return user;
}


#ifdef _MAIN

void test_user(int include_optional) {
    user_t* user_1 = instantiate_user(include_optional);

	cJSON* jsonuser_1 = user_convertToJSON(user_1);
	printf("user :\n%s\n", cJSON_Print(jsonuser_1));
	user_t* user_2 = user_parseFromJSON(jsonuser_1);
	cJSON* jsonuser_2 = user_convertToJSON(user_2);
	printf("repeating user:\n%s\n", cJSON_Print(jsonuser_2));
}

int main() {
  test_user(1);
  test_user(0);

  printf("Hello world \n");
  return 0;
}

#endif // _MAIN
#endif // _TEST

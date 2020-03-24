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


#include "../model/api_response.h"
api_response_t* instantiate_api_response(int include_optional);


api_response_t* instantiate_api_response(int include_optional) {
  api_response_t* api_response = api_response_create(
56
//primitive
, // code
"0"
//primitive
, // type
"0"
//primitive
 // message
  );
return api_response;
}


#ifdef _MAIN

void test_api_response(int include_optional) {
    api_response_t* api_response_1 = instantiate_api_response(include_optional);

	cJSON* jsonapi_response_1 = api_response_convertToJSON(api_response_1);
	printf("api_response :\n%s\n", cJSON_Print(jsonapi_response_1));
	api_response_t* api_response_2 = api_response_parseFromJSON(jsonapi_response_1);
	cJSON* jsonapi_response_2 = api_response_convertToJSON(api_response_2);
	printf("repeating api_response:\n%s\n", cJSON_Print(jsonapi_response_2));
}

int main() {
  test_api_response(1);
  test_api_response(0);

  printf("Hello world \n");
  return 0;
}

#endif // _MAIN
#endif // _TEST

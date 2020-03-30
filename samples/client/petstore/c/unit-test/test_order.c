#ifndef order_TEST
#define order_TEST

// the following is to include only the main from the first c file
#ifndef TEST_MAIN
#define TEST_MAIN
#define order_MAIN
#endif // TEST_MAIN

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "../external/cJSON.h"

#include "../model/order.h"
order_t* instantiate_order(int include_optional);



order_t* instantiate_order(int include_optional) {
  order_t* order = NULL;
  if (include_optional) {
    order = order_create(
      56,
      56,
      56,
      "2013-10-20T19:20:30+01:00",
      openapi_petstore_order_STATUS_placed,
      1
    );
  } else {
    order = order_create(
      56,
      56,
      56,
      "2013-10-20T19:20:30+01:00",
      openapi_petstore_order_STATUS_placed,
      1
    );
  }

  return order;
}


#ifdef order_MAIN

void test_order(int include_optional) {
    order_t* order_1 = instantiate_order(include_optional);

	cJSON* jsonorder_1 = order_convertToJSON(order_1);
	printf("order :\n%s\n", cJSON_Print(jsonorder_1));
	order_t* order_2 = order_parseFromJSON(jsonorder_1);
	cJSON* jsonorder_2 = order_convertToJSON(order_2);
	printf("repeating order:\n%s\n", cJSON_Print(jsonorder_2));
}

int main() {
  test_order(1);
  test_order(0);

  printf("Hello world \n");
  return 0;
}

#endif // order_MAIN
#endif // order_TEST

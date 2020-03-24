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


#include "../model/order.h"
order_t* instantiate_order(int include_optional);


order_t* instantiate_order(int include_optional) {
  order_t* order = order_create(
56
//primitive
, // id
56
//primitive
, // pet_id
56
//primitive
, // quantity
"2013-10-20T19:20:30+01:00"
//primitive
, // ship_date
openapi_petstore_order_STATUS_placed
//primitive
, // status
1
//primitive
 // complete
  );
return order;
}


#ifdef _MAIN

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

#endif // _MAIN
#endif // _TEST

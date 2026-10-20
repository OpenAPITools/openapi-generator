#ifndef bit_TEST
#define bit_TEST

// the following is to include only the main from the first c file
#ifndef TEST_MAIN
#define TEST_MAIN
#define bit_MAIN
#endif // TEST_MAIN

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include "../external/cJSON.h"

#include "../model/bit.h"
bit_t* instantiate_bit(int include_optional);



bit_t* instantiate_bit(int include_optional) {
  bit_t* bit = NULL;
  if (include_optional) {
    bit = bit_create(
    );
  } else {
    bit = bit_create(
    );
  }

  return bit;
}


#ifdef bit_MAIN

void test_bit(int include_optional) {
    bit_t* bit_1 = instantiate_bit(include_optional);

	cJSON* jsonbit_1 = bit_convertToJSON(bit_1);
	printf("bit :\n%s\n", cJSON_Print(jsonbit_1));
	bit_t* bit_2 = bit_parseFromJSON(jsonbit_1);
	cJSON* jsonbit_2 = bit_convertToJSON(bit_2);
	printf("repeating bit:\n%s\n", cJSON_Print(jsonbit_2));
}

int main() {
  test_bit(1);
  test_bit(0);

  printf("Hello world \n");
  return 0;
}

#endif // bit_MAIN
#endif // bit_TEST

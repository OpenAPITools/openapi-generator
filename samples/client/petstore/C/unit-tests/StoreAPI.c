#include <stdio.h>
    #define MAX_BUFFER_LENGTH 9
    #include <stdlib.h>
    #include <string.h>
    #include <assert.h>
    #include "apiClient.h"
    #include "cJSON.h"
    #include "order.h"
    #include "StoreAPI.h"
    #include "keyValuePair.h"

    #define ORDER_ID 1234
    #define PET_ID 12345
    #define QUANTITY 50
    #define SHIP_DATE "2018-09-24T10:19:09.592Z"
    #define STATUS "placed"
    #define COMPLETE true

/*
   Creates one pet and adds it. Then gets the pet with the just added ID and compare if the values are equal.
   Could fail if someone else makes changes to the added pet, before it can be fetched again.
 */
int main() {
	printf("Hello world1\n");
	apiClient_t *apiClient = apiClient_create();

	order_t *neworder = order_create(ORDER_ID,
	                                 PET_ID,
	                                 QUANTITY,
	                                 SHIP_DATE,
	                                 STATUS,
	                                 COMPLETE);

	order_t *returnorder = StoreAPI_placeOrder(apiClient, neworder);

	cJSON *JSONNODE = order_convertToJSON(returnorder);

	char *dataToPrint = cJSON_Print(JSONNODE);

	printf("Place order 1: \n%s\n", dataToPrint);
	apiClient_free(apiClient);

	printf(
		"------------------------------ Part Ends ----------------------------------\n");

	apiClient_t *apiClient2 = apiClient_create();

	neworder = StoreAPI_getOrderById(apiClient2, 1234);

	JSONNODE = order_convertToJSON(neworder);

	char *dataToPrint1 = cJSON_Print(JSONNODE);

	printf("Place order 2: \n%s\n", dataToPrint1);
	apiClient_free(apiClient2);

	printf(
		"------------------------------ Part Ends ----------------------------------\n");

	apiClient_t *apiClient3 = apiClient_create();

	StoreAPI_deleteOrder(apiClient3, "1234");

	printf("Order Deleted \n");
	apiClient_free(apiClient3);

	printf(
		"------------------------------ Part Ends ----------------------------------\n");

	apiClient_t *apiClient4 = apiClient_create();

	neworder = StoreAPI_getOrderById(apiClient4, 1234);

	if(neworder == NULL) {
		printf("Order Not present \n");
	}
	apiClient_free(apiClient4);
}

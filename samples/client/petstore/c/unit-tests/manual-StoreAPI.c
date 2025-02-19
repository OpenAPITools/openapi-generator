#include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <assert.h>
    #include "../api/StoreAPI.h"


    #define ORDER_ID 1234
    #define PET_ID 12345
    #define QUANTITY 50
    #define SHIP_DATE "2018-09-24T10:19:09.592Z"
    #define STATUS placed
    #define COMPLETE true

/*
   Creates one pet and adds it. Then gets the pet with the just added ID and compare if the values are equal.
   Could fail if someone else makes changes to the added pet, before it can be fetched again.
 */
int main() {
// place order test
	apiClient_t *apiClient = apiClient_create();

	char *shipdate = malloc(strlen(SHIP_DATE) + 1);
	strcpy(shipdate, SHIP_DATE);

	order_t *neworder = order_create(ORDER_ID,
	                                 PET_ID,
	                                 QUANTITY,
	                                 shipdate,
	                                 STATUS,
	                                 COMPLETE);

	order_t *returnorder = StoreAPI_placeOrder(apiClient, neworder);

	cJSON *JSONNODE = order_convertToJSON(returnorder);

	char *dataToPrint = cJSON_Print(JSONNODE);

	printf("Placed order: \n%s\n", dataToPrint);
	order_free(neworder);
	order_free(returnorder);
	cJSON_Delete(JSONNODE);
	free(dataToPrint);
	apiClient_free(apiClient);

// order get by id test
	apiClient_t *apiClient2 = apiClient_create();

	neworder = StoreAPI_getOrderById(apiClient2, 1234);

	JSONNODE = order_convertToJSON(neworder);

	char *dataToPrint1 = cJSON_Print(JSONNODE);

	printf("Order received: \n%s\n", dataToPrint1);

	order_free(neworder);
	cJSON_Delete(JSONNODE);
	free(dataToPrint1);
	apiClient_free(apiClient2);

// delete order test
	apiClient_t *apiClient3 = apiClient_create();

	char *orderid = malloc(strlen("1234") + 1);
	strcpy(orderid, "1234");

	StoreAPI_deleteOrder(apiClient3, orderid);

	printf("Order Deleted \n");
	free(orderid);
	apiClient_free(apiClient3);


// get order by id test
	apiClient_t *apiClient4 = apiClient_create();

	neworder = StoreAPI_getOrderById(apiClient4, 1234);

	if(neworder == NULL) {
		printf("Order Not present \n");
	}
	order_free(neworder);
	apiClient_free(apiClient4);

// get inventory test
	apiClient_t *apiClient5 = apiClient_create();
	list_t *elementToReturn;
	elementToReturn = StoreAPI_getInventory(apiClient5);
	listEntry_t *listEntry;
	list_ForEach(listEntry, elementToReturn) {
		keyValuePair_t *pair = (keyValuePair_t *) listEntry->data;
		printf("%s - %s\n", pair->key, (char *) pair->value);
	}
	list_ForEach(listEntry, elementToReturn) {
		keyValuePair_t *pair = (keyValuePair_t *) listEntry->data;
		free(pair->key);
		free(pair->value);
		keyValuePair_free(pair);
	}
	list_freeList(elementToReturn);
	apiClient_free(apiClient5);

	apiClient_unsetupGlobalEnv();
}

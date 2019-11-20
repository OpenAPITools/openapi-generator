#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "../model/order.h"


#define ORDER_ID 1234
#define PET_ID 12345
#define QUANTITY 50
#define SHIP_DATE "13/10/2018"

#define COMPLETE 1

int main() {
	status_e STATUS = placed;

	order_t *neworder = order_create(ORDER_ID, PET_ID, QUANTITY, SHIP_DATE,
	                                 STATUS,
	                                 COMPLETE);

	cJSON *JSONNODE = order_convertToJSON(neworder);

	char *dataToPrint = cJSON_Print(JSONNODE);

	printf("Created Order is: \n%s\n", dataToPrint);

	order_t *parsedOrder = order_parseFromJSON(JSONNODE);

	cJSON *fromJSON = order_convertToJSON(parsedOrder);

	char *dataToPrintFromJSON = cJSON_Print(fromJSON);

	printf("Parsed Order From JSON is: \n%s\n", dataToPrintFromJSON);

	order_free(neworder);
	order_free(parsedOrder);
	cJSON_Delete(JSONNODE);
	cJSON_Delete(fromJSON);
}

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "order.h"


    char* statusorder_ToString(status_e status){
    char *statusArray[] =  { "placed","approved","delivered" };
        return statusArray[status];
    }

    status_e statusorder_FromString(char* status){
    int stringToReturn = 0;
    char *statusArray[] =  { "placed","approved","delivered" };
    size_t sizeofArray = sizeof(statusArray) / sizeof(statusArray[0]);
    while(stringToReturn < sizeofArray) {
        if(strcmp(status, statusArray[stringToReturn]) == 0) {
            return stringToReturn;
        }
        stringToReturn++;
    }
    return 0;
    }

order_t *order_create(
    long id,
    long petId,
    int quantity,
    char *shipDate,
    status_e status,
    int complete
    ) {
	order_t *order_local_var = malloc(sizeof(order_t));
    if (!order_local_var) {
        return NULL;
    }
	order_local_var->id = id;
	order_local_var->petId = petId;
	order_local_var->quantity = quantity;
	order_local_var->shipDate = shipDate;
	order_local_var->status = status;
	order_local_var->complete = complete;

	return order_local_var;
}


void order_free(order_t *order) {
    listEntry_t *listEntry;
    free(order->shipDate);
	free(order);
}

cJSON *order_convertToJSON(order_t *order) {
	cJSON *item = cJSON_CreateObject();

	// order->id
    if(order->id) { 
    if(cJSON_AddNumberToObject(item, "id", order->id) == NULL) {
    goto fail; //Numeric
    }
     } 


	// order->petId
    if(order->petId) { 
    if(cJSON_AddNumberToObject(item, "petId", order->petId) == NULL) {
    goto fail; //Numeric
    }
     } 


	// order->quantity
    if(order->quantity) { 
    if(cJSON_AddNumberToObject(item, "quantity", order->quantity) == NULL) {
    goto fail; //Numeric
    }
     } 


	// order->shipDate
    if(order->shipDate) { 
    if(cJSON_AddStringToObject(item, "shipDate", order->shipDate) == NULL) {
    goto fail; //Date-Time
    }
     } 


	// order->status
    
    if(cJSON_AddStringToObject(item, "status", statusorder_ToString(order->status)) == NULL)
    {
    goto fail; //Enum
    }
    


	// order->complete
    if(order->complete) { 
    if(cJSON_AddBoolToObject(item, "complete", order->complete) == NULL) {
    goto fail; //Bool
    }
     } 

	return item;
fail:
	if (item) {
        cJSON_Delete(item);
    }
	return NULL;
}

order_t *order_parseFromJSON(cJSON *orderJSON){

    order_t *order_local_var = NULL;

    // order->id
    cJSON *id = cJSON_GetObjectItemCaseSensitive(orderJSON, "id");
    if (id) { 
    if(!cJSON_IsNumber(id))
    {
    goto end; //Numeric
    }
    }

    // order->petId
    cJSON *petId = cJSON_GetObjectItemCaseSensitive(orderJSON, "petId");
    if (petId) { 
    if(!cJSON_IsNumber(petId))
    {
    goto end; //Numeric
    }
    }

    // order->quantity
    cJSON *quantity = cJSON_GetObjectItemCaseSensitive(orderJSON, "quantity");
    if (quantity) { 
    if(!cJSON_IsNumber(quantity))
    {
    goto end; //Numeric
    }
    }

    // order->shipDate
    cJSON *shipDate = cJSON_GetObjectItemCaseSensitive(orderJSON, "shipDate");
    if (shipDate) { 
    if(!cJSON_IsString(shipDate))
    {
    goto end; //DateTime
    }
    }

    // order->status
    cJSON *status = cJSON_GetObjectItemCaseSensitive(orderJSON, "status");
    status_e statusVariable;
    if (status) { 
    if(!cJSON_IsString(status))
    {
    goto end; //Enum
    }
    statusVariable = statusorder_FromString(status->valuestring);
    }

    // order->complete
    cJSON *complete = cJSON_GetObjectItemCaseSensitive(orderJSON, "complete");
    if (complete) { 
    if(!cJSON_IsBool(complete))
    {
    goto end; //Bool
    }
    }


    order_local_var = order_create (
        id ? id->valuedouble : 0,
        petId ? petId->valuedouble : 0,
        quantity ? quantity->valuedouble : 0,
        shipDate ? strdup(shipDate->valuestring) : NULL,
        status ? statusVariable : -1,
        complete ? complete->valueint : 0
        );

    return order_local_var;
end:
    return NULL;

}

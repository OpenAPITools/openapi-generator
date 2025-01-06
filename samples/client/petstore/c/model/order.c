#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "order.h"


char* order_status_ToString(openapi_petstore_order_STATUS_e status) {
    char* statusArray[] =  { "NULL", "placed", "approved", "delivered" };
    return statusArray[status];
}

openapi_petstore_order_STATUS_e order_status_FromString(char* status){
    int stringToReturn = 0;
    char *statusArray[] =  { "NULL", "placed", "approved", "delivered" };
    size_t sizeofArray = sizeof(statusArray) / sizeof(statusArray[0]);
    while(stringToReturn < sizeofArray) {
        if(strcmp(status, statusArray[stringToReturn]) == 0) {
            return stringToReturn;
        }
        stringToReturn++;
    }
    return 0;
}

static order_t *order_create_internal(
    long id,
    long pet_id,
    int quantity,
    char *ship_date,
    openapi_petstore_order_STATUS_e status,
    int complete
    ) {
    order_t *order_local_var = malloc(sizeof(order_t));
    if (!order_local_var) {
        return NULL;
    }
    order_local_var->id = id;
    order_local_var->pet_id = pet_id;
    order_local_var->quantity = quantity;
    order_local_var->ship_date = ship_date;
    order_local_var->status = status;
    order_local_var->complete = complete;

    order_local_var->_library_owned = 1;
    return order_local_var;
}

__attribute__((deprecated)) order_t *order_create(
    long id,
    long pet_id,
    int quantity,
    char *ship_date,
    openapi_petstore_order_STATUS_e status,
    int complete
    ) {
    return order_create_internal (
        id,
        pet_id,
        quantity,
        ship_date,
        status,
        complete
        );
}

void order_free(order_t *order) {
    if(NULL == order){
        return ;
    }
    if(order->_library_owned != 1){
        fprintf(stderr, "WARNING: %s() does NOT free objects allocated by the user\n", "order_free");
        return ;
    }
    listEntry_t *listEntry;
    if (order->ship_date) {
        free(order->ship_date);
        order->ship_date = NULL;
    }
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


    // order->pet_id
    if(order->pet_id) {
    if(cJSON_AddNumberToObject(item, "petId", order->pet_id) == NULL) {
    goto fail; //Numeric
    }
    }


    // order->quantity
    if(order->quantity) {
    if(cJSON_AddNumberToObject(item, "quantity", order->quantity) == NULL) {
    goto fail; //Numeric
    }
    }


    // order->ship_date
    if(order->ship_date) {
    if(cJSON_AddStringToObject(item, "shipDate", order->ship_date) == NULL) {
    goto fail; //Date-Time
    }
    }


    // order->status
    if(order->status != openapi_petstore_order_STATUS_NULL) {
    if(cJSON_AddStringToObject(item, "status", order_status_ToString(order->status)) == NULL)
    {
    goto fail; //Enum
    }
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
    if (cJSON_IsNull(id)) {
        id = NULL;
    }
    if (id) { 
    if(!cJSON_IsNumber(id))
    {
    goto end; //Numeric
    }
    }

    // order->pet_id
    cJSON *pet_id = cJSON_GetObjectItemCaseSensitive(orderJSON, "petId");
    if (cJSON_IsNull(pet_id)) {
        pet_id = NULL;
    }
    if (pet_id) { 
    if(!cJSON_IsNumber(pet_id))
    {
    goto end; //Numeric
    }
    }

    // order->quantity
    cJSON *quantity = cJSON_GetObjectItemCaseSensitive(orderJSON, "quantity");
    if (cJSON_IsNull(quantity)) {
        quantity = NULL;
    }
    if (quantity) { 
    if(!cJSON_IsNumber(quantity))
    {
    goto end; //Numeric
    }
    }

    // order->ship_date
    cJSON *ship_date = cJSON_GetObjectItemCaseSensitive(orderJSON, "shipDate");
    if (cJSON_IsNull(ship_date)) {
        ship_date = NULL;
    }
    if (ship_date) { 
    if(!cJSON_IsString(ship_date) && !cJSON_IsNull(ship_date))
    {
    goto end; //DateTime
    }
    }

    // order->status
    cJSON *status = cJSON_GetObjectItemCaseSensitive(orderJSON, "status");
    if (cJSON_IsNull(status)) {
        status = NULL;
    }
    openapi_petstore_order_STATUS_e statusVariable;
    if (status) { 
    if(!cJSON_IsString(status))
    {
    goto end; //Enum
    }
    statusVariable = order_status_FromString(status->valuestring);
    }

    // order->complete
    cJSON *complete = cJSON_GetObjectItemCaseSensitive(orderJSON, "complete");
    if (cJSON_IsNull(complete)) {
        complete = NULL;
    }
    if (complete) { 
    if(!cJSON_IsBool(complete))
    {
    goto end; //Bool
    }
    }


    order_local_var = order_create_internal (
        id ? id->valuedouble : 0,
        pet_id ? pet_id->valuedouble : 0,
        quantity ? quantity->valuedouble : 0,
        ship_date && !cJSON_IsNull(ship_date) ? strdup(ship_date->valuestring) : NULL,
        status ? statusVariable : openapi_petstore_order_STATUS_NULL,
        complete ? complete->valueint : 0
        );

    return order_local_var;
end:
    return NULL;

}

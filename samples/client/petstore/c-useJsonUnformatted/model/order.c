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
    long *id,
    long *pet_id,
    int *quantity,
    char *ship_date,
    openapi_petstore_order_STATUS_e status,
    int *complete
    ) {
    order_t *order_local_var = malloc(sizeof(order_t));
    if (!order_local_var) {
        return NULL;
    }
    memset(order_local_var, 0, sizeof(order_t));
    order_local_var->_library_owned = 1;
    order_local_var->id = id;
    order_local_var->pet_id = pet_id;
    order_local_var->quantity = quantity;
    order_local_var->ship_date = ship_date;
    order_local_var->status = status;
    order_local_var->complete = complete;
    return order_local_var;
}

__attribute__((deprecated)) order_t *order_create(
    long *id,
    long *pet_id,
    int *quantity,
    char *ship_date,
    openapi_petstore_order_STATUS_e status,
    int *complete
    ) {
    long *id_copy = NULL;
    if (id) {
        id_copy = malloc(sizeof(long));
        if (id_copy) *id_copy = *id;
    }
    long *pet_id_copy = NULL;
    if (pet_id) {
        pet_id_copy = malloc(sizeof(long));
        if (pet_id_copy) *pet_id_copy = *pet_id;
    }
    int *quantity_copy = NULL;
    if (quantity) {
        quantity_copy = malloc(sizeof(int));
        if (quantity_copy) *quantity_copy = *quantity;
    }
    int *complete_copy = NULL;
    if (complete) {
        complete_copy = malloc(sizeof(int));
        if (complete_copy) *complete_copy = *complete;
    }
    order_t *result = order_create_internal (
        id_copy,
        pet_id_copy,
        quantity_copy,
        ship_date,
        status,
        complete_copy
        );
    if (!result) {
        free(id_copy);
        free(pet_id_copy);
        free(quantity_copy);
        free(complete_copy);
    }
    return result;
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
    if (order->id) {
        free(order->id);
        order->id = NULL;
    }
    if (order->pet_id) {
        free(order->pet_id);
        order->pet_id = NULL;
    }
    if (order->quantity) {
        free(order->quantity);
        order->quantity = NULL;
    }
    if (order->ship_date) {
        free(order->ship_date);
        order->ship_date = NULL;
    }
    if (order->complete) {
        free(order->complete);
        order->complete = NULL;
    }
    free(order);
}

cJSON *order_convertToJSON(order_t *order) {
    cJSON *item = cJSON_CreateObject();

    // order->id
    if(order->id) {
    if(cJSON_AddNumberToObject(item, "id", *order->id) == NULL) {
    goto fail; //Numeric
    }
    }


    // order->pet_id
    if(order->pet_id) {
    if(cJSON_AddNumberToObject(item, "petId", *order->pet_id) == NULL) {
    goto fail; //Numeric
    }
    }


    // order->quantity
    if(order->quantity) {
    if(cJSON_AddNumberToObject(item, "quantity", *order->quantity) == NULL) {
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
    if(cJSON_AddBoolToObject(item, "complete", *order->complete) == NULL) {
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

    // define the local variable for order->id
    long *id_local_var = NULL;

    // define the local variable for order->pet_id
    long *pet_id_local_var = NULL;

    // define the local variable for order->quantity
    int *quantity_local_var = NULL;

    // define the local variable for order->complete
    int *complete_local_var = NULL;

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
    id_local_var = malloc(sizeof(long));
    if(!id_local_var)
    {
        goto end;
    }
    *id_local_var = id->valuedouble;
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
    pet_id_local_var = malloc(sizeof(long));
    if(!pet_id_local_var)
    {
        goto end;
    }
    *pet_id_local_var = pet_id->valuedouble;
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
    quantity_local_var = malloc(sizeof(int));
    if(!quantity_local_var)
    {
        goto end;
    }
    *quantity_local_var = quantity->valuedouble;
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
    complete_local_var = malloc(sizeof(int));
    if(!complete_local_var)
    {
        goto end;
    }
    *complete_local_var = complete->valueint;
    }


    order_local_var = order_create_internal (
        id_local_var,
        pet_id_local_var,
        quantity_local_var,
        ship_date && !cJSON_IsNull(ship_date) ? strdup(ship_date->valuestring) : NULL,
        status ? statusVariable : openapi_petstore_order_STATUS_NULL,
        complete_local_var
        );

    if (!order_local_var) {
        goto end;
    }

    return order_local_var;
end:
    if (id_local_var) {
        free(id_local_var);
        id_local_var = NULL;
    }
    if (pet_id_local_var) {
        free(pet_id_local_var);
        pet_id_local_var = NULL;
    }
    if (quantity_local_var) {
        free(quantity_local_var);
        quantity_local_var = NULL;
    }
    if (complete_local_var) {
        free(complete_local_var);
        complete_local_var = NULL;
    }
    return NULL;

}

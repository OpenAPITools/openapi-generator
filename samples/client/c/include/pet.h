#include "list.h"
#include "category.h"

typedef enum status_t {available, pending, sold } status_t;

typedef struct pet_t {
    long id;
    category_t category;
    char *name;
    list_t photoUrls;
    list_t tags;
    status_t status;
} pet_t;

pet_t* pet_create();
void pet_free(pet_t* pet);
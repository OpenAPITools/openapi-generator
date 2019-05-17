#ifndef INCLUDE_LIST_H
#define INCLUDE_LIST_H

#include "../external/cJSON.h"
#include "../include/list.h"

typedef struct list_t list_t;

typedef struct listEntry_t listEntry_t;

struct listEntry_t {
    listEntry_t* nextListEntry;
    listEntry_t* prevListEntry;
    void* data;
};

typedef struct list_t {
    listEntry_t *firstEntry;
    listEntry_t *lastEntry;

    long count;
} list_t;

#define list_ForEach(element, list) for(element = (list != NULL) ? (list)->firstEntry : NULL; element != NULL; element = element->nextListEntry)

list_t* list_create();
void list_free(list_t* listToFree);

void list_addElement(list_t* list, void* dataToAddInList);
listEntry_t* list_getElementAt(list_t *list, long indexOfElement);
listEntry_t* list_getWithIndex(list_t* list, int index);
void list_removeElement(list_t* list, listEntry_t* elementToRemove);

void list_iterateThroughListForward(list_t* list, void (*operationToPerform)(listEntry_t*, void*), void *additionalDataNeededForCallbackFunction);
void list_iterateThroughListBackward(list_t* list, void (*operationToPerform)(listEntry_t*, void*), void *additionalDataNeededForCallbackFunction);

void listEntry_printAsInt(listEntry_t* listEntry, void *additionalData);
void listEntry_free(listEntry_t *listEntry, void *additionalData);
#endif // INCLUDE_LIST_H

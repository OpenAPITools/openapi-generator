#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../include/list.h"
static listEntry_t *listEntry_create(void *data) {
    listEntry_t *createdListEntry = malloc(sizeof(listEntry_t));
    if(createdListEntry == NULL) {
        // TODO Malloc Failure
        return NULL;
    }
    createdListEntry->data = data;

    return createdListEntry;
}

void listEntry_free(listEntry_t *listEntry, void *additionalData) {
    free(listEntry);
}

void listEntry_printAsInt(listEntry_t *listEntry, void *additionalData) {
    printf("%i\n", *((int *) (listEntry->data)));
}

list_t *list_createList() {
    list_t *createdList = malloc(sizeof(list_t));
    if(createdList == NULL) {
        // TODO Malloc Failure
        return NULL;
    }
    createdList->firstEntry = NULL;
    createdList->lastEntry = NULL;
    createdList->count = 0;

    return createdList;
}

void list_iterateThroughListForward(list_t *list,
                                    void (*operationToPerform)(
                                        listEntry_t *,
                                        void *callbackFunctionUsedData),
                                    void *additionalDataNeededForCallbackFunction)
{
    listEntry_t *currentListEntry = list->firstEntry;
    listEntry_t *nextListEntry;

    if(currentListEntry == NULL) {
        return;
    }

    nextListEntry = currentListEntry->nextListEntry;

    operationToPerform(currentListEntry,
                       additionalDataNeededForCallbackFunction);
    currentListEntry = nextListEntry;

    while(currentListEntry != NULL) {
        nextListEntry = currentListEntry->nextListEntry;
        operationToPerform(currentListEntry,
                           additionalDataNeededForCallbackFunction);
        currentListEntry = nextListEntry;
    }
}

void list_iterateThroughListBackward(list_t *list,
                                     void (*operationToPerform)(
                                         listEntry_t *,
                                         void *callbackFunctionUsedData),
                                     void *additionalDataNeededForCallbackFunction)
{
    listEntry_t *currentListEntry = list->lastEntry;
    listEntry_t *nextListEntry = currentListEntry->prevListEntry;

    if(currentListEntry == NULL) {
        return;
    }

    operationToPerform(currentListEntry,
                       additionalDataNeededForCallbackFunction);
    currentListEntry = nextListEntry;

    while(currentListEntry != NULL) {
        nextListEntry = currentListEntry->prevListEntry;
        operationToPerform(currentListEntry,
                           additionalDataNeededForCallbackFunction);
        currentListEntry = nextListEntry;
    }
}

void list_freeList(list_t *list) {
    if(list){
        list_iterateThroughListForward(list, listEntry_free, NULL);
        free(list);
    }
}

void list_addElement(list_t *list, void *dataToAddInList) {
    listEntry_t *newListEntry = listEntry_create(dataToAddInList);
    if(newListEntry == NULL) {
        // TODO Malloc Failure
        return;
    }
    if(list->firstEntry == NULL) {
        list->firstEntry = newListEntry;
        list->lastEntry = newListEntry;

        newListEntry->prevListEntry = NULL;
        newListEntry->nextListEntry = NULL;

        list->count++;

        return;
    }

    list->lastEntry->nextListEntry = newListEntry;
    newListEntry->prevListEntry = list->lastEntry;
    newListEntry->nextListEntry = NULL;
    list->lastEntry = newListEntry;

    list->count++;
}

void list_removeElement(list_t *list, listEntry_t *elementToRemove) {
    listEntry_t *elementBeforeElementToRemove =
        elementToRemove->prevListEntry;
    listEntry_t *elementAfterElementToRemove =
        elementToRemove->nextListEntry;

    if(elementBeforeElementToRemove != NULL) {
        elementBeforeElementToRemove->nextListEntry =
            elementAfterElementToRemove;
    } else {
        list->firstEntry = elementAfterElementToRemove;
    }

    if(elementAfterElementToRemove != NULL) {
        elementAfterElementToRemove->prevListEntry =
            elementBeforeElementToRemove;
    } else {
        list->lastEntry = elementBeforeElementToRemove;
    }

    listEntry_free(elementToRemove, NULL);

    list->count--;
}

listEntry_t *list_getElementAt(list_t *list, long indexOfElement) {
    listEntry_t *currentListEntry;

    if((list->count / 2) > indexOfElement) {
        currentListEntry = list->firstEntry;

        for(int i = 0; i < indexOfElement; i++) {
            currentListEntry = currentListEntry->nextListEntry;
        }

        return currentListEntry;
    } else {
        currentListEntry = list->lastEntry;

        for(int i = 1; i < (list->count - indexOfElement); i++) {
            currentListEntry = currentListEntry->prevListEntry;
        }

        return currentListEntry;
    }
}

char* findStrInStrList(list_t *strList, const char *str)
{
    if (!strList || !str) {
        return NULL;
    }

    listEntry_t* listEntry = NULL;
    list_ForEach(listEntry, strList) {
        if (strstr((char*)listEntry->data, str) != NULL) {
            return (char*)listEntry->data;
        }
    }

    return NULL;
}

void clear_and_free_string_list(list_t *list)
{
    if (!list) {
        return;
    }

    listEntry_t *listEntry = NULL;
    list_ForEach(listEntry, list) {
        char *list_item = listEntry->data;
        free(list_item);
        list_item = NULL;
    }
    list_freeList(list);
}
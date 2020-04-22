#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

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

list_t *list_create() {
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

void list_free(list_t *list) {
    list_iterateThroughListForward(list, listEntry_free, NULL);
    free(list);
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

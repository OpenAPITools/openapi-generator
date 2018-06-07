#include "list.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#define NUMBER_1 5
#define NUMBER_2 10
#define NUMBER_3 15

#define SEPARATOR "--------------------------------------"

void printSeparator() {
    puts(SEPARATOR);
}

int main() {
    long* number = malloc(sizeof(long));
    long* number2 = malloc(sizeof(long));
    long* number3 = malloc(sizeof(long));
    *number = NUMBER_1;
    *number2 = NUMBER_2;
    *number3 = NUMBER_3;

    list_t* myList = list_create();

    assert(myList->count == 0);    

    list_addElement(myList, number);
    list_addElement(myList, number2);

    printSeparator();
    
    list_iterateThroughListForward(myList, listEntry_printAsInt);

    printSeparator();

    list_iterateThroughListBackward(myList, listEntry_printAsInt);
    
    printSeparator();

    assert(* (int*) list_getElementAt(myList, 0)->data == NUMBER_1);
    assert(* (int*) list_getElementAt(myList, 1)->data == NUMBER_2);
    assert(myList->count == 2);

    list_removeElement(myList, list_getElementAt(myList, 0));

    assert(* (int*) list_getElementAt(myList, 0)->data == NUMBER_2);
    assert(myList->count == 1);

    list_removeElement(myList, list_getElementAt(myList, 0));

    assert(myList->count == 0);

    list_addElement(myList, number3);

    assert(myList->count == 1);
    listEntry_printAsInt(list_getElementAt(myList, 0));
    assert(* (int*) list_getElementAt(myList, 0)->data == NUMBER_3);

    list_free(myList);
}
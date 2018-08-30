#include "list.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#define NUMBER_1 5
#define NUMBER_2 10

#define SEPARATOR "--------------------------------------"

void printSeparator() {
	puts(SEPARATOR);
}

int main() {
	long *number1 = malloc(sizeof(long));
	long *number2 = malloc(sizeof(long));
	*number1 = NUMBER_1;
	*number2 = NUMBER_2;

	list_t *myList = list_create();

	assert(myList->count == 0);

	list_addElement(myList, number1);
	list_addElement(myList, number2);

	printSeparator();

	list_iterateThroughListForward(myList, listEntry_printAsInt, NULL);

	printSeparator();

	list_iterateThroughListBackward(myList, listEntry_printAsInt, NULL);

	printSeparator();

	assert(*(int *) list_getElementAt(myList, 0)->data == NUMBER_1);
	assert(*(int *) list_getElementAt(myList, 1)->data == NUMBER_2);
	assert(myList->count == 2);

	list_removeElement(myList, list_getElementAt(myList, 0));

	assert(*(int *) list_getElementAt(myList, 0)->data == NUMBER_2);
	assert(myList->count == 1);

	list_removeElement(myList, list_getElementAt(myList, 0));

	assert(myList->count == 0);

	list_addElement(myList, number2);

	assert(myList->count == 1);
	listEntry_printAsInt(list_getElementAt(myList, 0), NULL);
	assert(*(int *) list_getElementAt(myList, 0)->data == NUMBER_2);

	list_free(myList);

	free(number1);
	free(number2);
}
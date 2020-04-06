#include <stdlib.h>
#include <string.h>
#include "../include/binary.h"

binary_t* instantiate_binary_t(char* data, int len) {
	binary_t* ret = malloc(sizeof(struct binary_t));
	ret->len=len;
	ret->data = malloc(len);
	memcpy(ret->data, data, len);
	return ret;
}

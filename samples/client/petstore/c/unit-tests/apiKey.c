#include "keyValuePair.h"

int main() {
	keyValuePair_t *keyValuePair = keyValuePair_create("key", "value");
	keyValuePair_free(keyValuePair);
}
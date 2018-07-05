#include "apiKey.h"

int main() {
	apiKey_t *apiKey = apiKey_create("key", "value");
	apiKey_free(apiKey);
}
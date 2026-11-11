// Round trip for a boolean array: parse, inspect, serialize, free.
// Build from samples/client/petstore/c:
//   gcc unit-tests/manual-flags.c model/flags.c external/cJSON.c src/list.c -Iinclude -Iexternal -Imodel -o manual-flags && ./manual-flags
#include <assert.h>
#include <stdio.h>
#include "../external/cJSON.h"
#include "../model/flags.h"

int main(void) {
    cJSON *in = cJSON_Parse("{\"bits\":[true,false,true]}");
    assert(in);

    flags_t *f = flags_parseFromJSON(in);
    assert(f && f->bits && f->bits->count == 3);

    int expected[] = {1, 0, 1}, i = 0;
    listEntry_t *entry;
    list_ForEach(entry, f->bits) {
        assert(*(cJSON_bool *)entry->data == expected[i++]);
    }

    cJSON *out = flags_convertToJSON(f);
    assert(out);
    cJSON *arr = cJSON_GetObjectItemCaseSensitive(out, "bits");
    assert(cJSON_IsArray(arr) && cJSON_GetArraySize(arr) == 3);
    assert(cJSON_IsTrue(cJSON_GetArrayItem(arr, 0)));
    assert(cJSON_IsFalse(cJSON_GetArrayItem(arr, 1)));
    assert(cJSON_IsTrue(cJSON_GetArrayItem(arr, 2)));

    flags_free(f);
    cJSON_Delete(out);
    cJSON_Delete(in);

    // parse fails after the first item was stored: must not leak it
    cJSON *bad = cJSON_Parse("{\"bits\":[true,\"x\"]}");
    assert(bad);
    assert(flags_parseFromJSON(bad) == NULL);
    cJSON_Delete(bad);

    printf("manual-flags: OK\n");
    return 0;
}

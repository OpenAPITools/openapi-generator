#ifndef INCLUDE_TAG_H
#define INCLUDE_TAG_H

typedef struct tag_t {
    long id;
    char * name;
} tag_t;

tag_t *tag_create(long id, char *name);
void tag_free(tag_t * tag);

#endif //INCLUDE_TAG_H
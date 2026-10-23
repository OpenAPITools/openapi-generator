# PetApi

MicroProfile Rest Client interface. Obtain an implementation using `@RestClient`
injection or `RestClientBuilder`; the runtime provides HTTP transport and providers.

## createPet

`POST /pets`



- `` pet ``: `Pet` (required)

Returns `Uni<Pet>`.

## deletePet

`DELETE /pets/{petId}`



- `` petId ``: `kotlin.Long` (required)

Returns `Uni<Void?>`.

## echoNullable

`POST /echo`



- `` `value` ``: `kotlin.String?` (required)
- `` body ``: `kotlin.String?` (required)

Returns `Uni<Void?>`.

## getNullableName

`GET /maybe-name`




Returns `Uni<kotlin.String?>`.

## getPet

`GET /pets/{petId}`



- `` petId ``: `kotlin.Long` (required)
- `` filter ``: `kotlin.String? = null` (optional)
- `` xRequestId ``: `kotlin.String? = null` (optional)
- `` session ``: `kotlin.String? = null` (optional)

Returns `Uni<Pet>`.

## listPets

`GET /pets`



- `` tags ``: `kotlin.collections.List<kotlin.String>? = null` (optional)

Returns `Uni<kotlin.collections.List<Pet>>`.


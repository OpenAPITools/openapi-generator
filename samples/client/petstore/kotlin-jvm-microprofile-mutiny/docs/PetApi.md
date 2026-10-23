# PetApi

MicroProfile Rest Client interface. Obtain an implementation using `@RestClient`
injection or `RestClientBuilder`; the runtime provides HTTP transport and providers.

## createPet

`POST /pets`



- `pet`: `Pet` (required)

Returns `Uni<Pet>`.

## deletePet

`DELETE /pets/{petId}`



- `petId`: `kotlin.Long` (required)

Returns `Uni<Void?>`.

## echoNullable

`POST /echo`



- `&#x60;value&#x60;`: `kotlin.String` (required)
- `body`: `kotlin.String` (required)

Returns `Uni<Void?>`.

## getNullableName

`GET /maybe-name`




Returns `Uni<kotlin.String>`.

## getPet

`GET /pets/{petId}`



- `petId`: `kotlin.Long` (required)
- `filter`: `kotlin.String` (optional)
- `xRequestId`: `kotlin.String` (optional)
- `session`: `kotlin.String` (optional)

Returns `Uni<Pet>`.

## listPets

`GET /pets`



- `tags`: `kotlin.collections.List<kotlin.String>` (optional)

Returns `Uni<kotlin.collections.List<Pet>>`.


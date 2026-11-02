# PetApi

MicroProfile Rest Client interface. Obtain an implementation using `@RestClient`
injection or `RestClientBuilder`; the runtime provides HTTP transport and providers.

## createAnimal

`POST /animals`



- `` animal ``: `Animal` (required)

Returns `Uni<Animal>`.

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

## getAlternative

`GET /alternatives/{kind}`



- `` kind ``: `kotlin.String` (required)

Returns `Uni<Alternative>`.

## getAnimal

`GET /animals/{kind}`



- `` kind ``: `kotlin.String` (required)

Returns `Uni<Animal>`.

## getChoice

`GET /choices/{kind}`



- `` kind ``: `kotlin.String` (required)

Returns `Uni<Choice>`.

## getNullableName

`GET /maybe-name`




Returns `Uni<kotlin.String?>`.

## getNullablePet

`GET /maybe-pet`




Returns `Uni<Pet?>`.

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


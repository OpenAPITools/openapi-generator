# Dart-next Generator

This generator solves all the problems that other dart generators are facing, but rethinking the approaches we use to generate and represent APIs to closely match what the openapi spec expects.

Thus having 100% spec compliance and 0 surprises for the API consumer.

## Schema representation

The generator maps the following [schema types](https://spec.openapis.org/oas/v3.0.3#data-types) to their dart counterparts:

type| format | Comments | Dart type
------ | -------- | -------- | --------
`integer` | `int32` | signed 32 bits | `int`
`integer` | `int64` | signed 64 bits (a.k.a long) | `int`
`number` | `float` | | `double`
`number` | `double` | | `double`
`string` | | | `String`
`string` | `byte` | base64 encoded characters | `Uint8List`
`string` | `binary` | any sequence of octets | `XFile`
`boolean` | | | `bool`
`string` | `date` | As defined by `full-date` - [RFC3339](https://datatracker.ietf.org/doc/html/rfc3339#section-5.6) | `DateTime`
`string` | `date-time` | As defined by `date-time` - [RFC3339](https://datatracker.ietf.org/doc/html/rfc3339#section-5.6) | `DateTime`
`string` | `password` | A hint to UIs to obscure input. | `String`
`array` | | | `List<T>`
`map` | | | `Map<String, T>`
`object` | | | `$OpenApiObjectMixin` (more on that in [models](#models))

## Models

A model in this generator represents either:
1. an `object` type.
2. a `oneOf`/`anyOf` construct (even if it's between 2 pure primitives).
3. or both.

A model does NOT represent pure primitives (e.g. `type: string`), since they are represented with their own types (e.g. `String`).

All models are represented by classes that mix `$OpenApiObjectMixin`, and they MUST provide an override of `ModelReflection get $classReflection`.

## Reflections

The way this generator works, is by including the metadata described in the OAS document as part of the generated code as compile-time constants (`const`) that we call `Reflection`, and then using the generated reflections to handle serialization related functions.

Each reflection must provide the following functions:

1. `serializeFunction`: Takes in an instance of the associated type, and converts it to the appropriate serialization type based on the provided `SerializationContext`.
2. `canDeserializeFunction`: Takes in a serialization type, and checks if it can be converted to the associated type based on the provided `SerializationContext`.
3. `deserializeFunction`: Takes in a serialization type, and converts it to the associated type based on the provided `SerializationContext`.
4. `empty` generates an empty instance of the associated type. This is mainly used in deserialization if a property is required, we use the reflection to get an empty instance of it that MUST be changed later.
5. `example`: generates a random example value, used both in tests and in example folder.

There are 2 types of reflections and they both inherit `SerializationReflection<T>`, where `T` represents the dart type being reflected.

### Primitive Reflections

Used to reflect the primitive types described [above](#schema-representation).

### Class Reflections

Used to reflect [Models](#models) only.

Contains parts of the OAS document that are relevant to the class.

These are split into the 4 main types:

1. **PropertyPart**s: Used to describe property definitions, including their name, type, required, nullable, etc...
2. **additionalPropertiesPart**: Used to declare that this model accepts additional properties, and also contains information about their type.
3. **oneOfs**,**anyOfs**,**allOfs**: Used to describe composite relations in this model.

#### Property Parts


# Kotlin JAX-RS models and nullability

For `generatorName=kotlin-server`, `library=jaxrs-spec`, use
`interfaceOnly=true`, `useJakartaEe=true` and `useMutiny=true` to generate
Jakarta REST interfaces returning `Uni<T>`.
Nullable response schemas produce `Uni<T?>`; bodyless operations produce `Uni<Void?>`.

The default `inheritanceMode=abstract` supports discriminator-based `allOf`
inheritance and discriminated object `oneOf`/`anyOf` hierarchies. Jackson uses the
existing discriminator property, so serialization does not duplicate it. Keep
`fixJacksonJsonTypeInfoInheritance=true` for discriminated unions.
`none` flattens models and does not provide polymorphic deserialization;
`composition` produces `value: Any` wrappers, not a wire-compatible polymorphic
replacement. An undiscriminated `oneOf` remains an untyped alias in the default
inheritance mode, not a typed union.

Required nullable properties have nullable Kotlin types but must still be present
in JSON. Optional properties are nullable and default to null; omission and explicit
null are not distinguished. Bean Validation does not apply `@NotNull` to nullable
properties. Required non-null properties reject explicit nulls, including primitive
numbers. Register Jackson's Kotlin module in the application mapper for Kotlin
constructor/default-argument support, and enable its strict null checking when
validating non-null collection elements. Generated types and annotations do not
replace complete OpenAPI request/response validation.

See the [generated option reference](generators/kotlin-server.md) for configuration.

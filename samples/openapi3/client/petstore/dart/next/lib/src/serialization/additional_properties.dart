extension type const AdditionalProperties<T>._(Map<String, T> src)
    implements Map<String, dynamic> {
  const AdditionalProperties([Map<String, T> src = const {}])
      : this._(src);
  factory AdditionalProperties.srcExcept(
    Map<String, T> src,
    Set<String> toRemove,
  ) =>
      AdditionalProperties(
        Map.fromEntries(src.entries.where((e) => !toRemove.contains(e.key))),
      );
}

/// Mixin to mark objects as having additional properties.
mixin AdditionalPropertiesMixin<T> on $OpenApiObjectMixin {
  /// A special field containing properties that are not explicitly defined by the schema,
  /// but are included in the serialization process.
  ///
  /// These are NOT considered a separate field, but rather an extension of the schema.
  AdditionalProperties<T> get additionalProperties;
}

mixin $OpenApiObjectMixin {}
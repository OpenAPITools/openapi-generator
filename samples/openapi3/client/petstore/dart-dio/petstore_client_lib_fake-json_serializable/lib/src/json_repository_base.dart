class TypeInfo {
  final Type root;
  final bool nullable;
  final List<TypeInfo> parameters;

  /// The [Object] type.
  static const TypeInfo object = TypeInfo(Object);

  const TypeInfo(this.root, [this.parameters = const []]) : nullable = false;
  const TypeInfo.nullable(this.root, [this.parameters = const []])
      : nullable = true;

  @override
  bool operator ==(dynamic other) {
    if (identical(other, this)) return true;
    if (other is! TypeInfo) return false;
    if (root != other.root) return false;
    if (nullable != other.nullable) return false;
    if (parameters.length != other.parameters.length) return false;
    for (var i = 0; i != parameters.length; ++i) {
      if (parameters[i] != other.parameters[i]) return false;
    }
    return true;
  }

  @override
  int get hashCode {
    return Object.hash(root, Object.hashAll(parameters), nullable);
  }

  @override
  String toString() =>
      (parameters.isEmpty
          ? _getRawName(root)
          : '${_getRawName(root)}<${parameters.join(", ")}>') +
      _nullabilitySuffix;

  String get _nullabilitySuffix => nullable ? '?' : '';

  static String _getRawName(Type? type) {
    var name = type.toString();
    var genericsStart = name.indexOf('<');
    return genericsStart == -1 ? name : name.substring(0, genericsStart);
  }
}

abstract class SerializationRepositoryBase {
  Object serialize<T>(T src, TypeInfo parsedType, {Object? context});
  T deserialize<T>(Object value, TypeInfo parsedType, {Object? context});

  String encodeStringParameter<T>(T src, TypeInfo parsedType,
      {Object? context});
  Object encodeFormParameter<T>(T src, TypeInfo parsedType, {Object? context});
  Object encodeQueryParameter<T>(T src, TypeInfo parsedType, {Object? context});
}

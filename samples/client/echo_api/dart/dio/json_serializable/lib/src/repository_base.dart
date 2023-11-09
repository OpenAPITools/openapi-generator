/// The general rule for implementing this class is
/// json == serialize<T>(deserialize<T>(json))
/// object == deserialize<T>(serialize<T>(object))
/// Where json in this context means a dart primitive
/// JSON = String | num | bool | null | Uint8List | List<Json> | Map<String, Json>
abstract class SerializationRepositoryBase {
    const SerializationRepositoryBase();
    
    FutureOr<Object?> serialize<T>(T src, TypeInfo inputTypeInfo, {Object? context,});
    FutureOr<T> deserialize<T>(Object? value, TypeInfo targetTypeInfo, {Object? context,});
}

/// A modified version of `built_value`'s `FullType` class, found here
/// https://github.com/google/built_value.dart/blob/ee22d400a69c673330481a6da1f0796b90bb369b/built_value/lib/serializer.dart
/// It holds type information known by the spec (including generic type parameters)
class TypeInfo {
    final Type root;
    final bool nullable;
    final List<TypeInfo> parameters;

    /// The [Object] type.
    static const TypeInfo object = TypeInfo(Object);

    const TypeInfo(this.root, [this.parameters = const []]) : nullable = false;
    const TypeInfo.nullable(this.root, [this.parameters = const []]) : nullable = true;

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
    String toString() => (parameters.isEmpty
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

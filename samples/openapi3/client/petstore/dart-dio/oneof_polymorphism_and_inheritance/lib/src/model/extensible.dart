//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'extensible.g.dart';

/// Extensible
///
/// Properties:
/// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
/// * [atBaseType] - When sub-classing, this defines the super-class
/// * [atType] - When sub-classing, this defines the sub-class Extensible name
@BuiltValue(instantiable: false)
abstract class Extensible  {
  /// A URI to a JSON-Schema file that defines additional attributes and relationships
  @BuiltValueField(wireName: r'@schemaLocation')
  String? get atSchemaLocation;

  /// When sub-classing, this defines the super-class
  @BuiltValueField(wireName: r'@baseType')
  String? get atBaseType;

  /// When sub-classing, this defines the sub-class Extensible name
  @BuiltValueField(wireName: r'@type')
  String get atType;

  @BuiltValueSerializer(custom: true)
  static Serializer<Extensible> get serializer => _$ExtensibleSerializer();
}

class _$ExtensibleSerializer implements PrimitiveSerializer<Extensible> {
  @override
  final Iterable<Type> types = const [Extensible];

  @override
  final String wireName = r'Extensible';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Extensible object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.atSchemaLocation != null) {
      yield r'@schemaLocation';
      yield serializers.serialize(
        object.atSchemaLocation,
        specifiedType: const FullType(String),
      );
    }
    if (object.atBaseType != null) {
      yield r'@baseType';
      yield serializers.serialize(
        object.atBaseType,
        specifiedType: const FullType(String),
      );
    }
    yield r'@type';
    yield serializers.serialize(
      object.atType,
      specifiedType: const FullType(String),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    Extensible object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  @override
  Extensible deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.deserialize(serialized, specifiedType: FullType($Extensible)) as $Extensible;
  }
}

/// a concrete implementation of [Extensible], since [Extensible] is not instantiable
@BuiltValue(instantiable: true)
abstract class $Extensible implements Extensible, Built<$Extensible, $ExtensibleBuilder> {
  $Extensible._();

  factory $Extensible([void Function($ExtensibleBuilder)? updates]) = _$$Extensible;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults($ExtensibleBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<$Extensible> get serializer => _$$ExtensibleSerializer();
}

class _$$ExtensibleSerializer implements PrimitiveSerializer<$Extensible> {
  @override
  final Iterable<Type> types = const [$Extensible, _$$Extensible];

  @override
  final String wireName = r'$Extensible';

  @override
  Object serialize(
    Serializers serializers,
    $Extensible object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.serialize(object, specifiedType: FullType(Extensible))!;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ExtensibleBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'@schemaLocation':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.atSchemaLocation = valueDes;
          break;
        case r'@baseType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.atBaseType = valueDes;
          break;
        case r'@type':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.atType = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  $Extensible deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = $ExtensibleBuilder();
    final serializedList = (serialized as Iterable<Object?>).toList();
    final unhandled = <Object?>[];
    _deserializeProperties(
      serializers,
      serialized,
      specifiedType: specifiedType,
      serializedList: serializedList,
      unhandled: unhandled,
      result: result,
    );
    return result.build();
  }
}


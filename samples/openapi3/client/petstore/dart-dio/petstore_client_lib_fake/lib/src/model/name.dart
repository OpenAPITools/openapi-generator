//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'name.g.dart';

/// Model for testing model name same as property name
///
/// Properties:
/// * [name] 
/// * [snakeCase] 
/// * [property] 
/// * [n123number] 
@BuiltValue()
abstract class Name implements Built<Name, NameBuilder> {
  @BuiltValueField(wireName: r'name')
  int get name;

  @BuiltValueField(wireName: r'snake_case')
  int? get snakeCase;

  @BuiltValueField(wireName: r'property')
  String? get property;

  @BuiltValueField(wireName: r'123Number')
  int? get n123number;

  Name._();

  factory Name([void updates(NameBuilder b)]) = _$Name;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(NameBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Name> get serializer => _$NameSerializer();
}

class _$NameSerializer implements PrimitiveSerializer<Name> {
  @override
  final Iterable<Type> types = const [Name, _$Name];

  @override
  final String wireName = r'Name';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Name object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'name';
    yield serializers.serialize(
      object.name,
      specifiedType: const FullType(int),
    );
    if (object.snakeCase != null) {
      yield r'snake_case';
      yield serializers.serialize(
        object.snakeCase,
        specifiedType: const FullType(int),
      );
    }
    if (object.property != null) {
      yield r'property';
      yield serializers.serialize(
        object.property,
        specifiedType: const FullType(String),
      );
    }
    if (object.n123number != null) {
      yield r'123Number';
      yield serializers.serialize(
        object.n123number,
        specifiedType: const FullType(int),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Name object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required NameBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'name':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.name = valueDes;
          break;
        case r'snake_case':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.snakeCase = valueDes;
          break;
        case r'property':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.property = valueDes;
          break;
        case r'123Number':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.n123number = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Name deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = NameBuilder();
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


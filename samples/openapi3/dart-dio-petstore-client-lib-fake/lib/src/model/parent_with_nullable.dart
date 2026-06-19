//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/child_with_nullable.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'parent_with_nullable.g.dart';

/// ParentWithNullable
///
/// Properties:
/// * [type] 
/// * [nullableProperty] 
@BuiltValue(instantiable: false)
abstract class ParentWithNullable  {
  @BuiltValueField(wireName: r'type')
  String? get type;

  @BuiltValueField(wireName: r'nullableProperty')
  String? get nullableProperty;

  static const String discriminatorFieldName = r'type';

  static const Map<String, Type> discriminatorMapping = {
    r'ChildWithNullable': ChildWithNullable,
  };

  @BuiltValueSerializer(custom: true)
  static Serializer<ParentWithNullable> get serializer => _$ParentWithNullableSerializer();
}

extension ParentWithNullableDiscriminatorExt on ParentWithNullable {
    String? get discriminatorValue {
        if (this is ChildWithNullable) {
            return r'ChildWithNullable';
        }
        return null;
    }
}
extension ParentWithNullableBuilderDiscriminatorExt on ParentWithNullableBuilder {
    String? get discriminatorValue {
        if (this is ChildWithNullableBuilder) {
            return r'ChildWithNullable';
        }
        return null;
    }
}

class _$ParentWithNullableSerializer implements PrimitiveSerializer<ParentWithNullable> {
  @override
  final Iterable<Type> types = const [ParentWithNullable];

  @override
  final String wireName = r'ParentWithNullable';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ParentWithNullable object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.type != null) {
      yield r'type';
      yield serializers.serialize(
        object.type,
        specifiedType: const FullType(String),
      );
    }
    if (object.nullableProperty != null) {
      yield r'nullableProperty';
      yield serializers.serialize(
        object.nullableProperty,
        specifiedType: const FullType.nullable(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ParentWithNullable object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    if (object is ChildWithNullable) {
      return serializers.serialize(object, specifiedType: FullType(ChildWithNullable))!;
    }
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  @override
  ParentWithNullable deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(ParentWithNullable.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    switch (discValue) {
      case r'ChildWithNullable':
        return serializers.deserialize(serialized, specifiedType: FullType(ChildWithNullable)) as ChildWithNullable;
      default:
        return serializers.deserialize(serialized, specifiedType: FullType($ParentWithNullable)) as $ParentWithNullable;
    }
  }
}

/// a concrete implementation of [ParentWithNullable], since [ParentWithNullable] is not instantiable
@BuiltValue(instantiable: true)
abstract class $ParentWithNullable implements ParentWithNullable, Built<$ParentWithNullable, $ParentWithNullableBuilder> {
  $ParentWithNullable._();

  factory $ParentWithNullable([void Function($ParentWithNullableBuilder)? updates]) = _$$ParentWithNullable;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults($ParentWithNullableBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<$ParentWithNullable> get serializer => _$$ParentWithNullableSerializer();
}

class _$$ParentWithNullableSerializer implements PrimitiveSerializer<$ParentWithNullable> {
  @override
  final Iterable<Type> types = const [$ParentWithNullable, _$$ParentWithNullable];

  @override
  final String wireName = r'$ParentWithNullable';

  @override
  Object serialize(
    Serializers serializers,
    $ParentWithNullable object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.serialize(object, specifiedType: FullType(ParentWithNullable))!;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ParentWithNullableBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'type':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.type = valueDes;
          break;
        case r'nullableProperty':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(String),
          ) as String?;
          if (valueDes == null) continue;
          result.nullableProperty = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  $ParentWithNullable deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = $ParentWithNullableBuilder();
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


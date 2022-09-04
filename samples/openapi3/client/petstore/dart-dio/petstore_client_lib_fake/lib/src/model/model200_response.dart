//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model200_response.g.dart';

/// Model for testing model name starting with number
///
/// Properties:
/// * [name] 
/// * [classField] 
@BuiltValue()
abstract class Model200Response implements Built<Model200Response, Model200ResponseBuilder> {
  @BuiltValueField(wireName: r'name')
  int? get name;

  @BuiltValueField(wireName: r'class')
  String? get classField;

  Model200Response._();

  factory Model200Response([void updates(Model200ResponseBuilder b)]) = _$Model200Response;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(Model200ResponseBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Model200Response> get serializer => _$Model200ResponseSerializer();
}

class _$Model200ResponseSerializer implements PrimitiveSerializer<Model200Response> {
  @override
  final Iterable<Type> types = const [Model200Response, _$Model200Response];

  @override
  final String wireName = r'Model200Response';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Model200Response object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.name != null) {
      yield r'name';
      yield serializers.serialize(
        object.name,
        specifiedType: const FullType(int),
      );
    }
    if (object.classField != null) {
      yield r'class';
      yield serializers.serialize(
        object.classField,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Model200Response object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required Model200ResponseBuilder result,
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
        case r'class':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.classField = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Model200Response deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = Model200ResponseBuilder();
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


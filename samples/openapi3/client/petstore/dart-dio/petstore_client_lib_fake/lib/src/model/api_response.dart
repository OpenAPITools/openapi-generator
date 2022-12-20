//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'api_response.g.dart';

/// ApiResponse
///
/// Properties:
/// * [code] 
/// * [type] 
/// * [message] 
@BuiltValue()
abstract class ApiResponse implements Built<ApiResponse, ApiResponseBuilder> {
  @BuiltValueField(wireName: r'code')
  int? get code;

  @BuiltValueField(wireName: r'type')
  String? get type;

  @BuiltValueField(wireName: r'message')
  String? get message;

  ApiResponse._();

  factory ApiResponse([void updates(ApiResponseBuilder b)]) = _$ApiResponse;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ApiResponseBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ApiResponse> get serializer => _$ApiResponseSerializer();
}

class _$ApiResponseSerializer implements PrimitiveSerializer<ApiResponse> {
  @override
  final Iterable<Type> types = const [ApiResponse, _$ApiResponse];

  @override
  final String wireName = r'ApiResponse';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ApiResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.code != null) {
      yield r'code';
      yield serializers.serialize(
        object.code,
        specifiedType: const FullType(int),
      );
    }
    if (object.type != null) {
      yield r'type';
      yield serializers.serialize(
        object.type,
        specifiedType: const FullType(String),
      );
    }
    if (object.message != null) {
      yield r'message';
      yield serializers.serialize(
        object.message,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ApiResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ApiResponseBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'code':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.code = valueDes;
          break;
        case r'type':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.type = valueDes;
          break;
        case r'message':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.message = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ApiResponse deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ApiResponseBuilder();
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


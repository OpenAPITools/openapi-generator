//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'validation_error_response.g.dart';

/// ValidationErrorResponse
///
/// Properties:
/// * [attr] - The attribute that caused the validation error
@BuiltValue()
abstract class ValidationErrorResponse implements Built<ValidationErrorResponse, ValidationErrorResponseBuilder> {
  /// The attribute that caused the validation error
  @BuiltValueField(wireName: r'attr')
  ValidationErrorResponseAttrEnum? get attr;
  // enum attrEnum {  name,  username,  };

  ValidationErrorResponse._();

  factory ValidationErrorResponse([void updates(ValidationErrorResponseBuilder b)]) = _$ValidationErrorResponse;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ValidationErrorResponseBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ValidationErrorResponse> get serializer => _$ValidationErrorResponseSerializer();
}

class _$ValidationErrorResponseSerializer implements PrimitiveSerializer<ValidationErrorResponse> {
  @override
  final Iterable<Type> types = const [ValidationErrorResponse, _$ValidationErrorResponse];

  @override
  final String wireName = r'ValidationErrorResponse';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ValidationErrorResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.attr != null) {
      yield r'attr';
      yield serializers.serialize(
        object.attr,
        specifiedType: const FullType(ValidationErrorResponseAttrEnum),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ValidationErrorResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ValidationErrorResponseBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'attr':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(ValidationErrorResponseAttrEnum),
          ) as ValidationErrorResponseAttrEnum;
          result.attr = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ValidationErrorResponse deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ValidationErrorResponseBuilder();
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

class ValidationErrorResponseAttrEnum extends EnumClass {

  /// The attribute that caused the validation error
  @BuiltValueEnumConst(wireName: r'name')
  static const ValidationErrorResponseAttrEnum nameAttr = _$validationErrorResponseAttrEnum_name;
  /// The attribute that caused the validation error
  @BuiltValueEnumConst(wireName: r'username')
  static const ValidationErrorResponseAttrEnum usernameAttr = _$validationErrorResponseAttrEnum_username;
  /// The attribute that caused the validation error
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const ValidationErrorResponseAttrEnum unknownDefaultOpenApiAttr = _$validationErrorResponseAttrEnum_unknownDefaultOpenApi;

  static Serializer<ValidationErrorResponseAttrEnum> get serializer => _$validationErrorResponseAttrEnumSerializer;

  const ValidationErrorResponseAttrEnum._(String name): super(name);

  static BuiltSet<ValidationErrorResponseAttrEnum> get values => _$validationErrorResponseAttrEnumValues;
  static ValidationErrorResponseAttrEnum valueOf(String name) => _$validationErrorResponseAttrEnumValueOf(name);
}


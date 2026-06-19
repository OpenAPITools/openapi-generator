//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/json_object.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'test_inline_freeform_additional_properties_request.g.dart';

/// TestInlineFreeformAdditionalPropertiesRequest
///
/// Properties:
/// * [someProperty] 
@BuiltValue()
abstract class TestInlineFreeformAdditionalPropertiesRequest implements Built<TestInlineFreeformAdditionalPropertiesRequest, TestInlineFreeformAdditionalPropertiesRequestBuilder> {
  @BuiltValueField(wireName: r'someProperty')
  String? get someProperty;

  TestInlineFreeformAdditionalPropertiesRequest._();

  factory TestInlineFreeformAdditionalPropertiesRequest([void updates(TestInlineFreeformAdditionalPropertiesRequestBuilder b)]) = _$TestInlineFreeformAdditionalPropertiesRequest;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(TestInlineFreeformAdditionalPropertiesRequestBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<TestInlineFreeformAdditionalPropertiesRequest> get serializer => _$TestInlineFreeformAdditionalPropertiesRequestSerializer();
}

class _$TestInlineFreeformAdditionalPropertiesRequestSerializer implements PrimitiveSerializer<TestInlineFreeformAdditionalPropertiesRequest> {
  @override
  final Iterable<Type> types = const [TestInlineFreeformAdditionalPropertiesRequest, _$TestInlineFreeformAdditionalPropertiesRequest];

  @override
  final String wireName = r'TestInlineFreeformAdditionalPropertiesRequest';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    TestInlineFreeformAdditionalPropertiesRequest object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.someProperty != null) {
      yield r'someProperty';
      yield serializers.serialize(
        object.someProperty,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    TestInlineFreeformAdditionalPropertiesRequest object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required TestInlineFreeformAdditionalPropertiesRequestBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'someProperty':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.someProperty = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  TestInlineFreeformAdditionalPropertiesRequest deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = TestInlineFreeformAdditionalPropertiesRequestBuilder();
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


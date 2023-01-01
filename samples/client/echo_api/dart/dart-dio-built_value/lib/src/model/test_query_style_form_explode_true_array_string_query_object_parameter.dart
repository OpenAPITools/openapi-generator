//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'test_query_style_form_explode_true_array_string_query_object_parameter.g.dart';

/// TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
///
/// Properties:
/// * [values] 
@BuiltValue()
abstract class TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter implements Built<TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter, TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterBuilder> {
  @BuiltValueField(wireName: r'values')
  BuiltList<String>? get values;

  TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter._();

  factory TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter([void updates(TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterBuilder b)]) = _$TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter> get serializer => _$TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterSerializer();
}

class _$TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterSerializer implements PrimitiveSerializer<TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter> {
  @override
  final Iterable<Type> types = const [TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter, _$TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter];

  @override
  final String wireName = r'TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.values != null) {
      yield r'values';
      yield serializers.serialize(
        object.values,
        specifiedType: const FullType(BuiltList, [FullType(String)]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'values':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltList, [FullType(String)]),
          ) as BuiltList<String>;
          result.values.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterBuilder();
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


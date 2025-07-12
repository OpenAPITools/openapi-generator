//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/test_enum.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'test_item.g.dart';

/// TestItem
///
/// Properties:
/// * [test] 
/// * [testEmum] 
@BuiltValue()
abstract class TestItem implements Built<TestItem, TestItemBuilder> {
  @BuiltValueField(wireName: r'test')
  int get test;

  @BuiltValueField(wireName: r'testEmum')
  TestEnum? get testEmum;
  // enum testEmumEnum {  ,  1,  2,  };

  TestItem._();

  factory TestItem([void updates(TestItemBuilder b)]) = _$TestItem;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(TestItemBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<TestItem> get serializer => _$TestItemSerializer();
}

class _$TestItemSerializer implements PrimitiveSerializer<TestItem> {
  @override
  final Iterable<Type> types = const [TestItem, _$TestItem];

  @override
  final String wireName = r'TestItem';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    TestItem object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'test';
    yield serializers.serialize(
      object.test,
      specifiedType: const FullType(int),
    );
    if (object.testEmum != null) {
      yield r'testEmum';
      yield serializers.serialize(
        object.testEmum,
        specifiedType: const FullType(TestEnum),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    TestItem object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required TestItemBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'test':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.test = valueDes;
          break;
        case r'testEmum':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(TestEnum),
          ) as TestEnum;
          result.testEmum = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  TestItem deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = TestItemBuilder();
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


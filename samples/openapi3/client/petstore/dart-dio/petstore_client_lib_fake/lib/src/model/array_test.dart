//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/read_only_first.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'array_test.g.dart';

/// ArrayTest
///
/// Properties:
/// * [arrayOfString] 
/// * [arrayWithUniqueItems] 
/// * [arrayArrayOfInteger] 
/// * [arrayArrayOfModel] 
@BuiltValue()
abstract class ArrayTest implements Built<ArrayTest, ArrayTestBuilder> {
  @BuiltValueField(wireName: r'array_of_string')
  BuiltList<String>? get arrayOfString;

  @BuiltValueField(wireName: r'array_with_unique_items')
  BuiltSet<ArrayTestArrayWithUniqueItemsEnum>? get arrayWithUniqueItems;
  // enum arrayWithUniqueItemsEnum {  unique_item_1,  unique_item_2,  unique_item_3,  };

  @BuiltValueField(wireName: r'array_array_of_integer')
  BuiltList<BuiltList<int>>? get arrayArrayOfInteger;

  @BuiltValueField(wireName: r'array_array_of_model')
  BuiltList<BuiltList<ReadOnlyFirst>>? get arrayArrayOfModel;

  ArrayTest._();

  factory ArrayTest([void updates(ArrayTestBuilder b)]) = _$ArrayTest;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ArrayTestBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ArrayTest> get serializer => _$ArrayTestSerializer();
}

class _$ArrayTestSerializer implements PrimitiveSerializer<ArrayTest> {
  @override
  final Iterable<Type> types = const [ArrayTest, _$ArrayTest];

  @override
  final String wireName = r'ArrayTest';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ArrayTest object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.arrayOfString != null) {
      yield r'array_of_string';
      yield serializers.serialize(
        object.arrayOfString,
        specifiedType: const FullType(BuiltList, [FullType(String)]),
      );
    }
    if (object.arrayWithUniqueItems != null) {
      yield r'array_with_unique_items';
      yield serializers.serialize(
        object.arrayWithUniqueItems,
        specifiedType: const FullType(BuiltSet, [FullType(ArrayTestArrayWithUniqueItemsEnum)]),
      );
    }
    if (object.arrayArrayOfInteger != null) {
      yield r'array_array_of_integer';
      yield serializers.serialize(
        object.arrayArrayOfInteger,
        specifiedType: const FullType(BuiltList, [FullType(BuiltList, [FullType(int)])]),
      );
    }
    if (object.arrayArrayOfModel != null) {
      yield r'array_array_of_model';
      yield serializers.serialize(
        object.arrayArrayOfModel,
        specifiedType: const FullType(BuiltList, [FullType(BuiltList, [FullType(ReadOnlyFirst)])]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ArrayTest object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ArrayTestBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'array_of_string':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltList, [FullType(String)]),
          ) as BuiltList<String>;
          result.arrayOfString.replace(valueDes);
          break;
        case r'array_with_unique_items':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltSet, [FullType(ArrayTestArrayWithUniqueItemsEnum)]),
          ) as BuiltSet<ArrayTestArrayWithUniqueItemsEnum>;
          result.arrayWithUniqueItems.replace(valueDes);
          break;
        case r'array_array_of_integer':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltList, [FullType(BuiltList, [FullType(int)])]),
          ) as BuiltList<BuiltList<int>>;
          result.arrayArrayOfInteger.replace(valueDes);
          break;
        case r'array_array_of_model':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltList, [FullType(BuiltList, [FullType(ReadOnlyFirst)])]),
          ) as BuiltList<BuiltList<ReadOnlyFirst>>;
          result.arrayArrayOfModel.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ArrayTest deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ArrayTestBuilder();
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

class ArrayTestArrayWithUniqueItemsEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'unique_item_1')
  static const ArrayTestArrayWithUniqueItemsEnum n1 = _$arrayTestArrayWithUniqueItemsEnum_n1;
  @BuiltValueEnumConst(wireName: r'unique_item_2')
  static const ArrayTestArrayWithUniqueItemsEnum n2 = _$arrayTestArrayWithUniqueItemsEnum_n2;
  @BuiltValueEnumConst(wireName: r'unique_item_3')
  static const ArrayTestArrayWithUniqueItemsEnum n3 = _$arrayTestArrayWithUniqueItemsEnum_n3;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const ArrayTestArrayWithUniqueItemsEnum unknownDefaultOpenApi = _$arrayTestArrayWithUniqueItemsEnum_unknownDefaultOpenApi;

  static Serializer<ArrayTestArrayWithUniqueItemsEnum> get serializer => _$arrayTestArrayWithUniqueItemsEnumSerializer;

  const ArrayTestArrayWithUniqueItemsEnum._(String name): super(name);

  static BuiltSet<ArrayTestArrayWithUniqueItemsEnum> get values => _$arrayTestArrayWithUniqueItemsEnumValues;
  static ArrayTestArrayWithUniqueItemsEnum valueOf(String name) => _$arrayTestArrayWithUniqueItemsEnumValueOf(name);
}


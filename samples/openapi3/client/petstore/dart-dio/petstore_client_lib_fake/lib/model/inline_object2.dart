import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'inline_object2.g.dart';

abstract class InlineObject2 implements Built<InlineObject2, InlineObject2Builder> {

    /// Form parameter enum test (string array)
    @nullable
    @BuiltValueField(wireName: r'enum_form_string_array')
    BuiltList<InlineObject2EnumFormStringArrayEnum> get enumFormStringArray;
    // enum enumFormStringArrayEnum {  >,  $,  };

    /// Form parameter enum test (string)
    @nullable
    @BuiltValueField(wireName: r'enum_form_string')
    InlineObject2EnumFormStringEnum get enumFormString;
    // enum enumFormStringEnum {  _abc,  -efg,  (xyz),  };

    // Boilerplate code needed to wire-up generated code
    InlineObject2._();

    static void _initializeBuilder(InlineObject2Builder b) => b
        ..enumFormString = const InlineObject2EnumFormStringEnum._('-efg');

    factory InlineObject2([void updates(InlineObject2Builder b)]) = _$InlineObject2;
    static Serializer<InlineObject2> get serializer => _$inlineObject2Serializer;
}

class InlineObject2EnumFormStringArrayEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'>')
  static const InlineObject2EnumFormStringArrayEnum greaterThan = _$inlineObject2EnumFormStringArrayEnum_greaterThan;
  @BuiltValueEnumConst(wireName: r'\$')
  static const InlineObject2EnumFormStringArrayEnum dollar = _$inlineObject2EnumFormStringArrayEnum_dollar;

  static Serializer<InlineObject2EnumFormStringArrayEnum> get serializer => _$inlineObject2EnumFormStringArrayEnumSerializer;

  const InlineObject2EnumFormStringArrayEnum._(String name): super(name);

  static BuiltSet<InlineObject2EnumFormStringArrayEnum> get values => _$inlineObject2EnumFormStringArrayEnumValues;
  static InlineObject2EnumFormStringArrayEnum valueOf(String name) => _$inlineObject2EnumFormStringArrayEnumValueOf(name);
}

class InlineObject2EnumFormStringEnum extends EnumClass {

  /// Form parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'_abc')
  static const InlineObject2EnumFormStringEnum abc = _$inlineObject2EnumFormStringEnum_abc;
  /// Form parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'-efg')
  static const InlineObject2EnumFormStringEnum efg = _$inlineObject2EnumFormStringEnum_efg;
  /// Form parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'(xyz)')
  static const InlineObject2EnumFormStringEnum leftParenthesisXyzRightParenthesis = _$inlineObject2EnumFormStringEnum_leftParenthesisXyzRightParenthesis;

  static Serializer<InlineObject2EnumFormStringEnum> get serializer => _$inlineObject2EnumFormStringEnumSerializer;

  const InlineObject2EnumFormStringEnum._(String name): super(name);

  static BuiltSet<InlineObject2EnumFormStringEnum> get values => _$inlineObject2EnumFormStringEnumValues;
  static InlineObject2EnumFormStringEnum valueOf(String name) => _$inlineObject2EnumFormStringEnumValueOf(name);
}


import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'inline_object2.g.dart';

abstract class InlineObject2 implements Built<InlineObject2, InlineObject2Builder> {

    /* Form parameter enum test (string array) */
    @nullable
    @BuiltValueField(wireName: r'enum_form_string_array')
    InlineObject2EnumFormStringArray get enumFormStringArray;
    // enum enumFormStringArrayEnum {  &gt;,  $,  };
    /* Form parameter enum test (string) */
    @nullable
    @BuiltValueField(wireName: r'enum_form_string')
    InlineObject2EnumFormString get enumFormString;
    // enum enumFormStringEnum {  _abc,  -efg,  (xyz),  };

    // Boilerplate code needed to wire-up generated code
    InlineObject2._();

    factory InlineObject2([updates(InlineObject2Builder b)]) = _$InlineObject2;
    static Serializer<InlineObject2> get serializer => _$inlineObject2Serializer;
}

class InlineObject2EnumFormStringArray extends EnumClass {

  /// Form parameter enum test (string array)
  @BuiltValueEnumConst(wireName: r'>')
  static const InlineObject2EnumFormStringArray greaterThan = _$inlineObject2EnumFormStringArray_greaterThan;
  /// Form parameter enum test (string array)
  @BuiltValueEnumConst(wireName: r'\$')
  static const InlineObject2EnumFormStringArray dollar = _$inlineObject2EnumFormStringArray_dollar;

  static Serializer<InlineObject2EnumFormStringArray> get serializer => _$inlineObject2EnumFormStringArraySerializer;

  const InlineObject2EnumFormStringArray._(String name): super(name);

  static BuiltSet<InlineObject2EnumFormStringArray> get values => _$inlineObject2EnumFormStringArrayValues;
  static InlineObject2EnumFormStringArray valueOf(String name) => _$inlineObject2EnumFormStringArrayValueOf(name);
}


class InlineObject2EnumFormString extends EnumClass {

  /// Form parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'_abc')
  static const InlineObject2EnumFormString abc = _$inlineObject2EnumFormString_abc;
  /// Form parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'-efg')
  static const InlineObject2EnumFormString efg = _$inlineObject2EnumFormString_efg;
  /// Form parameter enum test (string)
  @BuiltValueEnumConst(wireName: r'(xyz)')
  static const InlineObject2EnumFormString leftParenthesisXyzRightParenthesis = _$inlineObject2EnumFormString_leftParenthesisXyzRightParenthesis;

  static Serializer<InlineObject2EnumFormString> get serializer => _$inlineObject2EnumFormStringSerializer;

  const InlineObject2EnumFormString._(String name): super(name);

  static BuiltSet<InlineObject2EnumFormString> get values => _$inlineObject2EnumFormStringValues;
  static InlineObject2EnumFormString valueOf(String name) => _$inlineObject2EnumFormStringValueOf(name);
}



//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_enum_class.g.dart';

class ModelEnumClass extends EnumClass {

  @BuiltValueEnumConst(wireName: r'_abc')
  static const ModelEnumClass abc = _$abc;
  @BuiltValueEnumConst(wireName: r'-efg')
  static const ModelEnumClass efg = _$efg;
  @BuiltValueEnumConst(wireName: r'(xyz)')
  static const ModelEnumClass leftParenthesisXyzRightParenthesis = _$leftParenthesisXyzRightParenthesis;

  static Serializer<ModelEnumClass> get serializer => _$modelEnumClassSerializer;

  const ModelEnumClass._(String name): super(name);

  static BuiltSet<ModelEnumClass> get values => _$values;
  static ModelEnumClass valueOf(String name) => _$valueOf(name);
}

/// Optionally, enum_class can generate a mixin to go with your enum for use
/// with Angular. It exposes your enum constants as getters. So, if you mix it
/// in to your Dart component class, the values become available to the
/// corresponding Angular template.
///
/// Trigger mixin generation by writing a line like this one next to your enum.
abstract class ModelEnumClassMixin = Object with _$ModelEnumClassMixin;


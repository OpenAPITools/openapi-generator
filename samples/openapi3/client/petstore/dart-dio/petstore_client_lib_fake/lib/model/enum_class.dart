import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'enum_class.g.dart';

class EnumClass extends EnumClass {

  @BuiltValueEnumConst(wireName: '_abc')
  static const EnumClass abc = _$abc;
  @BuiltValueEnumConst(wireName: '-efg')
  static const EnumClass efg = _$efg;
  @BuiltValueEnumConst(wireName: '(xyz)')
  static const EnumClass (xyz) = _$(xyz);

  static Serializer<EnumClass> get serializer => _$enumClassSerializer;

  const EnumClass._(String name): super(name);

  static BuiltSet<EnumClass> get values => _$values;
  static EnumClass valueOf(String name) => _$valueOf(name);
}

/// Optionally, enum_class can generate a mixin to go with your enum for use
/// with Angular. It exposes your enum constants as getters. So, if you mix it
/// in to your Dart component class, the values become available to the
/// corresponding Angular template.
///
/// Trigger mixin generation by writing a line like this one next to your enum.
abstract class EnumClassMixin = Object with _$EnumClassMixin;


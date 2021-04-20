//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'outer_enum_integer.g.dart';

class OuterEnumInteger extends EnumClass {

  @BuiltValueEnumConst(wireNumber: 0)
  static const OuterEnumInteger number0 = _$number0;
  @BuiltValueEnumConst(wireNumber: 1)
  static const OuterEnumInteger number1 = _$number1;
  @BuiltValueEnumConst(wireNumber: 2)
  static const OuterEnumInteger number2 = _$number2;

  static Serializer<OuterEnumInteger> get serializer => _$outerEnumIntegerSerializer;

  const OuterEnumInteger._(String name): super(name);

  static BuiltSet<OuterEnumInteger> get values => _$values;
  static OuterEnumInteger valueOf(String name) => _$valueOf(name);
}

/// Optionally, enum_class can generate a mixin to go with your enum for use
/// with Angular. It exposes your enum constants as getters. So, if you mix it
/// in to your Dart component class, the values become available to the
/// corresponding Angular template.
///
/// Trigger mixin generation by writing a line like this one next to your enum.
abstract class OuterEnumIntegerMixin = Object with _$OuterEnumIntegerMixin;


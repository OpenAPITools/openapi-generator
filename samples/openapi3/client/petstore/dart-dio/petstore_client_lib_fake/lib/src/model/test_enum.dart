//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'test_enum.g.dart';

class TestEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'')
  static const TestEnum empty = _$empty;
  @BuiltValueEnumConst(wireName: r'1')
  static const TestEnum n1 = _$n1;
  @BuiltValueEnumConst(wireName: r'2')
  static const TestEnum n2 = _$n2;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const TestEnum unknownDefaultOpenApi = _$unknownDefaultOpenApi;

  static Serializer<TestEnum> get serializer => _$testEnumSerializer;

  const TestEnum._(String name): super(name);

  static BuiltSet<TestEnum> get values => _$values;
  static TestEnum valueOf(String name) => _$valueOf(name);
}

/// Optionally, enum_class can generate a mixin to go with your enum for use
/// with Angular. It exposes your enum constants as getters. So, if you mix it
/// in to your Dart component class, the values become available to the
/// corresponding Angular template.
///
/// Trigger mixin generation by writing a line like this one next to your enum.
abstract class TestEnumMixin = Object with _$TestEnumMixin;


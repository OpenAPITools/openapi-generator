//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'fruit_type.g.dart';

class FruitType extends EnumClass {

  @BuiltValueEnumConst(wireName: r'APPLE')
  static const FruitType APPLE = _$APPLE;
  @BuiltValueEnumConst(wireName: r'BANANA')
  static const FruitType BANANA = _$BANANA;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const FruitType unknownDefaultOpenApi = _$unknownDefaultOpenApi;

  static Serializer<FruitType> get serializer => _$fruitTypeSerializer;

  const FruitType._(String name): super(name);

  static BuiltSet<FruitType> get values => _$values;
  static FruitType valueOf(String name) => _$valueOf(name);
}

/// Optionally, enum_class can generate a mixin to go with your enum for use
/// with Angular. It exposes your enum constants as getters. So, if you mix it
/// in to your Dart component class, the values become available to the
/// corresponding Angular template.
///
/// Trigger mixin generation by writing a line like this one next to your enum.
abstract class FruitTypeMixin = Object with _$FruitTypeMixin;


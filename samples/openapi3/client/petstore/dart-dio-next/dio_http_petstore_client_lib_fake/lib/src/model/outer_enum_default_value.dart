//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'outer_enum_default_value.g.dart';

class OuterEnumDefaultValue extends EnumClass {

  @BuiltValueEnumConst(wireName: r'placed')
  static const OuterEnumDefaultValue placed = _$placed;
  @BuiltValueEnumConst(wireName: r'approved')
  static const OuterEnumDefaultValue approved = _$approved;
  @BuiltValueEnumConst(wireName: r'delivered')
  static const OuterEnumDefaultValue delivered = _$delivered;

  static Serializer<OuterEnumDefaultValue> get serializer => _$outerEnumDefaultValueSerializer;

  const OuterEnumDefaultValue._(String name): super(name);

  static BuiltSet<OuterEnumDefaultValue> get values => _$values;
  static OuterEnumDefaultValue valueOf(String name) => _$valueOf(name);
}

/// Optionally, enum_class can generate a mixin to go with your enum for use
/// with Angular. It exposes your enum constants as getters. So, if you mix it
/// in to your Dart component class, the values become available to the
/// corresponding Angular template.
///
/// Trigger mixin generation by writing a line like this one next to your enum.
abstract class OuterEnumDefaultValueMixin = Object with _$OuterEnumDefaultValueMixin;


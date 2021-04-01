//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'outer_enum.g.dart';

class OuterEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'placed')
  static const OuterEnum placed = _$placed;
  @BuiltValueEnumConst(wireName: r'approved')
  static const OuterEnum approved = _$approved;
  @BuiltValueEnumConst(wireName: r'delivered')
  static const OuterEnum delivered = _$delivered;

  static Serializer<OuterEnum> get serializer => _$outerEnumSerializer;

  const OuterEnum._(String name): super(name);

  static BuiltSet<OuterEnum> get values => _$values;
  static OuterEnum valueOf(String name) => _$valueOf(name);
}

/// Optionally, enum_class can generate a mixin to go with your enum for use
/// with Angular. It exposes your enum constants as getters. So, if you mix it
/// in to your Dart component class, the values become available to the
/// corresponding Angular template.
///
/// Trigger mixin generation by writing a line like this one next to your enum.
abstract class OuterEnumMixin = Object with _$OuterEnumMixin;


// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'outer_enum_default_value.reflection.dart';
part 'outer_enum_default_value.serialization.dart';


//enum def

extension type const OuterEnumDefaultValue._(String value) {
      const OuterEnumDefaultValue.placed() : this._(r'placed');
      const OuterEnumDefaultValue.approved() : this._(r'approved');
      const OuterEnumDefaultValue.delivered() : this._(r'delivered');

  /// Creates a [OuterEnumDefaultValue] enum from a value and safely checking if it exists.
  factory OuterEnumDefaultValue.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [OuterEnumDefaultValue] enum from a value without checking if it exists.
  const OuterEnumDefaultValue.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<OuterEnumDefaultValue> values = [
    OuterEnumDefaultValue.placed(),
    OuterEnumDefaultValue.approved(),
    OuterEnumDefaultValue.delivered(),
    
  ];
}
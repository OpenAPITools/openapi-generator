// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'outer_enum_integer.reflection.dart';
part 'outer_enum_integer.serialization.dart';

//enum def

extension type const OuterEnumInteger._(int value) {
  const OuterEnumInteger.number0() : this._(0);
  const OuterEnumInteger.number1() : this._(1);
  const OuterEnumInteger.number2() : this._(2);

  /// Creates a [OuterEnumInteger] enum from a value and safely checking if it exists.
  factory OuterEnumInteger.$safe(int value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [OuterEnumInteger] enum from a value without checking if it exists.
  const OuterEnumInteger.$unsafe(int value) : this._(value);

  /// All possible values of the enum.
  static const List<OuterEnumInteger> values = [
    OuterEnumInteger.number0(),
    OuterEnumInteger.number1(),
    OuterEnumInteger.number2(),
  ];
}

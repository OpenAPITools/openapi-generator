// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'outer_enum.reflection.dart';
part 'outer_enum.serialization.dart';

//enum def

extension type const OuterEnum._(String value) {
  const OuterEnum.placed() : this._(r'placed');
  const OuterEnum.approved() : this._(r'approved');
  const OuterEnum.delivered() : this._(r'delivered');
  const OuterEnum.LOWER_CASE_S() : this._(r's');
  const OuterEnum.UPPER_CASE_S() : this._(r'S');

  /// Creates a [OuterEnum] enum from a value and safely checking if it exists.
  factory OuterEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [OuterEnum] enum from a value without checking if it exists.
  const OuterEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<OuterEnum> values = [
    OuterEnum.placed(),
    OuterEnum.approved(),
    OuterEnum.delivered(),
    OuterEnum.LOWER_CASE_S(),
    OuterEnum.UPPER_CASE_S(),
  ];
}

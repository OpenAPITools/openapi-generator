// Model def

import 'package:openapi/_internal.dart';


part 'enum_class.reflection.dart';
part 'enum_class.serialization.dart';


//enum def

extension type const EnumClass._(String value) {
      const EnumClass.abc() : this._(r'_abc');
      const EnumClass.efg() : this._(r'-efg');
      const EnumClass.xyz() : this._(r'(xyz)');

  /// Creates a [EnumClass] enum from a value and safely checking if it exists.
  factory EnumClass.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumClass] enum from a value without checking if it exists.
  const EnumClass.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumClass> values = [
    EnumClass.abc(),
    EnumClass.efg(),
    EnumClass.xyz(),
    
  ];
}
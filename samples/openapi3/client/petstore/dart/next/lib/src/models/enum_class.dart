// Model def

import 'package:petstore_api/_internal.dart';


part 'enum_class.reflection.dart';


//enum def

extension type const EnumClass._(String value) implements String {
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

  static const $reflection = EnumReflection(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'abc', oasValue: r'_abc', value: EnumClass.abc()),
      
        EnumMemberReflection(dartName: r'efg', oasValue: r'-efg', value: EnumClass.efg()),
      
        EnumMemberReflection(dartName: r'xyz', oasValue: r'(xyz)', value: EnumClass.xyz()),
      
    ],
  );

  factory EnumClass.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
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
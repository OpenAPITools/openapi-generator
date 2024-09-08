// Model def

import 'package:petstore_api/_internal.dart';


part 'outer_enum_default_value.reflection.dart';


//enum def

extension type const OuterEnumDefaultValue._(String value) implements String {
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

  static const $reflection = EnumReflection(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'placed', oasValue: r'placed', value: OuterEnumDefaultValue.placed()),
      
        EnumMemberReflection(dartName: r'approved', oasValue: r'approved', value: OuterEnumDefaultValue.approved()),
      
        EnumMemberReflection(dartName: r'delivered', oasValue: r'delivered', value: OuterEnumDefaultValue.delivered()),
      
    ],
  );

  factory OuterEnumDefaultValue.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
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
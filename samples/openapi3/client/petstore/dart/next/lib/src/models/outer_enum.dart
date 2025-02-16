// Model def

import 'package:petstore_api/_internal.dart';


part 'outer_enum.reflection.dart';


//enum def

extension type const OuterEnum._(String value) implements String {
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

  static const $reflection = EnumReflection(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'placed', oasValue: r'placed', value: OuterEnum.placed()),
      
        EnumMemberReflection(dartName: r'approved', oasValue: r'approved', value: OuterEnum.approved()),
      
        EnumMemberReflection(dartName: r'delivered', oasValue: r'delivered', value: OuterEnum.delivered()),
      
        EnumMemberReflection(dartName: r'LOWER_CASE_S', oasValue: r's', value: OuterEnum.LOWER_CASE_S()),
      
        EnumMemberReflection(dartName: r'UPPER_CASE_S', oasValue: r'S', value: OuterEnum.UPPER_CASE_S()),
      
    ],
  );

  factory OuterEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
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
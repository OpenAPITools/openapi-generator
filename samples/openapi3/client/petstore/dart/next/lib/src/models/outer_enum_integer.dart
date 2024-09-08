// Model def

import 'package:petstore_api/_internal.dart';


part 'outer_enum_integer.reflection.dart';


//enum def

extension type const OuterEnumInteger._(int value) implements int {
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

  static const $reflection = EnumReflection(
    PrimitiveReflection.forint,
    members: [
      
        EnumMemberReflection(dartName: r'number0', oasValue: 0, value: OuterEnumInteger.number0()),
      
        EnumMemberReflection(dartName: r'number1', oasValue: 1, value: OuterEnumInteger.number1()),
      
        EnumMemberReflection(dartName: r'number2', oasValue: 2, value: OuterEnumInteger.number2()),
      
    ],
  );

  factory OuterEnumInteger.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
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
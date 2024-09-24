// Model def

import 'package:petstore_api/_internal.dart';


part 'outer_enum_integer_default_value.reflection.dart';


//enum def

extension type const OuterEnumIntegerDefaultValue._(int value) implements int {
      const OuterEnumIntegerDefaultValue.number0() : this._(0);
      const OuterEnumIntegerDefaultValue.number1() : this._(1);
      const OuterEnumIntegerDefaultValue.number2() : this._(2);

  /// Creates a [OuterEnumIntegerDefaultValue] enum from a value and safely checking if it exists.
  factory OuterEnumIntegerDefaultValue.$safe(int value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static const $reflection = EnumReflection(
    PrimitiveReflection.forint,
    members: [
      
        EnumMemberReflection(dartName: r'number0', oasValue: 0, value: OuterEnumIntegerDefaultValue.number0()),
      
        EnumMemberReflection(dartName: r'number1', oasValue: 1, value: OuterEnumIntegerDefaultValue.number1()),
      
        EnumMemberReflection(dartName: r'number2', oasValue: 2, value: OuterEnumIntegerDefaultValue.number2()),
      
    ],
  );

  factory OuterEnumIntegerDefaultValue.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [OuterEnumIntegerDefaultValue] enum from a value without checking if it exists.
  const OuterEnumIntegerDefaultValue.$unsafe(int value) : this._(value);

  /// All possible values of the enum.
  static const List<OuterEnumIntegerDefaultValue> values = [
    OuterEnumIntegerDefaultValue.number0(),
    OuterEnumIntegerDefaultValue.number1(),
    OuterEnumIntegerDefaultValue.number2(),
    
  ];
}
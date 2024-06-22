abstract class ClassReflection<T> {
  const ClassReflection();
  List<PropertyReflection> get members;
  Set<String> get knownKeys => members.map((e) => e.oasName).toSet();

  T Function(Object? src) get deserializeFunction;
  bool Function(Object? src) get canDeserializeFunction;
  Object? Function(T src) get serializeFunction;
}

class PropertyReflection<T> {
  const PropertyReflection({
    required this.dartName,
    required this.oasName,
    required this.oasType,
    required this.required,
    required this.nullable,
    this.$default,
    this.pattern,
  });

  final String dartName;
  final String oasName;
  final String oasType;
  final bool required;
  final bool nullable;
  final Object? $default;
  final String? pattern;
}

// abstract class EnumReflection<T extends Enum> {
//   const EnumReflection();
//   Type get type => T;
//   List<EnumMemberReflection> get members;
// }

class EnumMemberReflection {
  const EnumMemberReflection({
    required this.dartName,
    required this.oasValue,
  });
  final String dartName;
  final Object? oasValue;
}

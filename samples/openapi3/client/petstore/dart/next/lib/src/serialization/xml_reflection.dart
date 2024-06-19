import 'package:xml/xml.dart';

abstract class XmlClassReflection<T> {
  const XmlClassReflection();
  Type get type => T;

  List<XmlElementReflection> get children;
  List<XmlAttributeReflection> get attributes;

  T Function(XmlElement json) get fromXmlFunction;
  XmlElement Function(T src) get toXmlFunction;
}

abstract class XmlPartReflectionBase {
  final String dartName;
  final String? oasNamespace;
  final String oasName;
  final String oasType;
  final bool required;
  final bool nullable;

  const XmlPartReflectionBase({
    required this.dartName,
    required this.oasNamespace,
    required this.oasName,
    required this.oasType,
    required this.required,
    required this.nullable,
  });
}

class XmlElementReflection extends XmlPartReflectionBase {
  const XmlElementReflection({
    required super.dartName,
    required super.oasName,
    required super.oasType,
    required super.required,
    required super.nullable,
    super.oasNamespace,
    this.$default,
    this.pattern,
  });

  final String? $default;
  final String? pattern;
}

class XmlAttributeReflection extends XmlPartReflectionBase {
  const XmlAttributeReflection({
    required super.dartName,
    required super.oasName,
    super.oasNamespace,
    required super.oasType,
    required super.required,
    required super.nullable,
  });
}

// abstract class XmlEnumReflection<T extends Enum> {
//   const XmlEnumReflection();
//   Type get type => T;
//   List<XmlEnumMemberReflection> get members;
// }

class XmlEnumMemberReflection {
  const XmlEnumMemberReflection({
    required this.dartName,
    required this.oasValue,
  });
  final String dartName;
  final String oasValue;
}

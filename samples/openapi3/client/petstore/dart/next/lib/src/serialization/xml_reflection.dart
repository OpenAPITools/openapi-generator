import 'package:petstore_api/_internal.dart';

mixin HasXmlReflection {
  XmlReflection get xml;
}

class XmlReflection {
  const XmlReflection({
    this.xmlName,
    this.namespace,
    this.prefix,
    this.attribute = false,
    this.wrapped = false,
  });

  final String? xmlName;
  final String? namespace;
  final String? prefix;
  final bool attribute;
  final bool wrapped;

  XmlName getQualifiedName(String fallbackName) {
    return XmlName(xmlName ?? fallbackName, prefix);
  }

  XmlName? getQualifiedNameOrNull(String? fallbackName) {
    final actualName = xmlName ?? fallbackName;
    if (actualName == null) {
      return null;
    }
    return XmlName(actualName, prefix);
  }

  XmlAttribute? getXmlNamespaceAttribute() {
    if (prefix == null || namespace == null) {
      return null;
    }
    return XmlAttribute(XmlName(prefix!, 'xmlns'), namespace!);
  }
}
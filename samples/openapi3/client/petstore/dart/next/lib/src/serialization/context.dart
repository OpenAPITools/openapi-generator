import 'package:petstore_api/_internal.dart';

enum SupportedWireTypes {
  other,
  json,
  xml,
}

int xmlNameHashcode(XmlName a) {
  return Object.hash(a.local, a.namespaceUri, a.prefix);
}

bool xmlNameEquals(XmlName a, XmlName b) {
  return a.local == b.local &&
      a.namespaceUri == b.namespaceUri &&
      a.prefix == b.prefix;
}

typedef SerializeReflectionResolver = SerializationReflection<Object?>? Function(
  Object src,
);
typedef DeserializeReflectionResolver = SerializationReflection<Object>?
    Function(
  Object? src,
);
typedef ValueConverterFunction = Object? Function(Object? value);
typedef ValueConverterCheckerFunction = bool Function(Object? value);
typedef FileBytesSyncResolver = Uint8List? Function(XFile file);

/// This function is used to check if a generic type T1 can be assigned to a generic type T2.
///
/// e.g. a value of type int can be assigned to a value of type num
/// isOfType<int, num>() == true
/// but not the opposite
/// isOfType<num, int>() == true
///
/// This also works for deeply nested objects
/// isOfType<List<List<int>>, List<List<num>>>() == true
bool isOfType<T1, T2>() {
  return (_fakeFunction<T1>) is T2 Function();
}

TItem Function() extractIterableType<T extends Iterable<TItem>, TItem>() {
  return _fakeFunction<TItem>;
}

/// This function is used to check if a generic type T can accept a null value.
bool isNullableType<T>() {
  return null is T;
}

/// This function will NEVER be called.
T _fakeFunction<T>() {
  return null as T;
}

abstract class SerializationContext {
  const SerializationContext({
    this.serializeReflectionResolver,
    this.deserializeReflectionResolver,
    this.fileBytesResolver,
  });

  SerializationContext copyWith({
    SerializeReflectionResolver? serializeReflectionResolver,
    DeserializeReflectionResolver? deserializeReflectionResolver,
    FileBytesSyncResolver? fileBytesResolver,
  });

  /// Creates a json serialization context, where the target is to output
  /// types that can be passed to jsonEncode and jsonDecode.
  ///
  /// You can completely control the serialization and deserialization process
  /// for unknown objects by passing [serializeReflectionResolver] and [deserializeReflectionResolver].
  const factory SerializationContext.json({
    SerializeReflectionResolver? serializeReflectionResolver,
    DeserializeReflectionResolver? deserializeReflectionResolver,
    FileBytesSyncResolver? fileBytesResolver,
  }) = JsonSerializationContext;

  /// Creates an XML serialization context, where the target is to output
  /// an XmlNode.
  ///
  /// You can completely control the serialization and deserialization process
  /// for unknown objects by passing [serializeReflectionResolver] and [deserializeReflectionResolver].
  ///
  /// [xmlContainer] and [oasNameContainer] are required here due to the nature
  /// of XML, since serializing a value requires information about the property
  /// name, namespace, prefix, wrapped, etc...
  const factory SerializationContext.xml({
    SerializeReflectionResolver? serializeReflectionResolver,
    DeserializeReflectionResolver? deserializeReflectionResolver,
    FileBytesSyncResolver? fileBytesResolver,
    HasXmlReflection? xmlContainer,
    HasOasName? oasNameContainer,
  }) = XmlSerializationContext;

  // used when serializing an unknown value to discover its reflection.
  final SerializeReflectionResolver? serializeReflectionResolver;
  // used when deserializing an unknown value to discover its reflection.
  final DeserializeReflectionResolver? deserializeReflectionResolver;

  /// Try to resolve the bytes of a file synchronously, if not possible, return null.
  final FileBytesSyncResolver? fileBytesResolver;

  SupportedWireTypes get wireType;
  bool get isJson => wireType == SupportedWireTypes.json;
  bool get isXml => wireType == SupportedWireTypes.xml;

  T split<T>({
    required T Function(JsonSerializationContext context) onJson,
    required T Function(XmlSerializationContext context) onXml,
    T Function(SerializationContext context)? onOther,
  }) {
    return switch (this) {
      JsonSerializationContext json => onJson(json),
      XmlSerializationContext xml => onXml(xml),
      _ => onOther == null ? throw UnimplementedError() : onOther(this),
    };
  }
}

class JsonSerializationContext extends SerializationContext {
  const JsonSerializationContext({
    super.serializeReflectionResolver,
    super.deserializeReflectionResolver,
    super.fileBytesResolver,
  });

  @override
  SupportedWireTypes get wireType => SupportedWireTypes.json;

  @override
  JsonSerializationContext copyWith({
    SerializeReflectionResolver? serializeReflectionResolver,
    DeserializeReflectionResolver? deserializeReflectionResolver,
    FileBytesSyncResolver? fileBytesResolver,
  }) =>
      JsonSerializationContext(
        serializeReflectionResolver:
            serializeReflectionResolver ?? this.serializeReflectionResolver,
        deserializeReflectionResolver:
            deserializeReflectionResolver ?? this.deserializeReflectionResolver,
        fileBytesResolver: fileBytesResolver ?? this.fileBytesResolver,
      );
}

class XmlSerializationContext extends SerializationContext {
  const XmlSerializationContext({
    super.serializeReflectionResolver,
    super.deserializeReflectionResolver,
    super.fileBytesResolver,
    this.xmlContainer,
    this.oasNameContainer,
  });

  static final xmlnsAttribute = XmlAttribute(
    XmlName("xmlns"),
    'http://www.w3.org/2001/XMLSchema',
  );
  static final xsiNamespaceAttribute = XmlAttribute(
    XmlName("xsi", "xmlns"),
    'http://www.w3.org/2001/XMLSchema-instance',
  );
  static final nilXmlName = XmlName("nil", "xsi");
  static final nilXmlAttribute = XmlAttribute(nilXmlName, 'true');
  static final nilXmlElement = XmlElement(nilXmlName, [], [XmlText("true")]);

  @override
  SupportedWireTypes get wireType => SupportedWireTypes.xml;

  final HasXmlReflection? xmlContainer;
  final HasOasName? oasNameContainer;

  XmlSerializationContext withXmlContainer(HasXmlReflection? xmlContainer) {
    return XmlSerializationContext(
      serializeReflectionResolver: serializeReflectionResolver,
      deserializeReflectionResolver: deserializeReflectionResolver,
      fileBytesResolver: fileBytesResolver,
      xmlContainer: xmlContainer,
      oasNameContainer: oasNameContainer,
    );
  }

  XmlSerializationContext withOasNameContainer(HasOasName? oasNameContainer) {
    return XmlSerializationContext(
      serializeReflectionResolver: serializeReflectionResolver,
      deserializeReflectionResolver: deserializeReflectionResolver,
      fileBytesResolver: fileBytesResolver,
      xmlContainer: xmlContainer,
      oasNameContainer: oasNameContainer,
    );
  }

  @override
  XmlSerializationContext copyWith({
    SerializeReflectionResolver? serializeReflectionResolver,
    DeserializeReflectionResolver? deserializeReflectionResolver,
    FileBytesSyncResolver? fileBytesResolver,
    HasXmlReflection? xmlContainer,
    HasOasName? oasNameContainer,
  }) =>
      XmlSerializationContext(
        serializeReflectionResolver:
            serializeReflectionResolver ?? this.serializeReflectionResolver,
        deserializeReflectionResolver:
            deserializeReflectionResolver ?? this.deserializeReflectionResolver,
        fileBytesResolver: fileBytesResolver ?? this.fileBytesResolver,
        xmlContainer: xmlContainer ?? this.xmlContainer,
        oasNameContainer: oasNameContainer ?? this.oasNameContainer,
      );


  XmlNode wrapSerializedValue(
    XmlName key,
    Object? value, {
    XmlReflection? keyReflection,
  }) {
    key = key.copy();
    final keyXmlns = keyReflection?.getXmlNamespaceAttribute();
    switch (value) {
      case null:
        return XmlElement(key, [
          XmlSerializationContext.nilXmlAttribute.copy(),
          if (keyXmlns != null) keyXmlns,
        ]);
      case XmlNode node:
        return XmlElement(key, [
          if (keyXmlns != null) keyXmlns,
        ], [
          node.copy()
        ]);
      case MapEntry<XmlReflection, Object?> nodeEntry:
        final subXml = nodeEntry.key;
        var item = nodeEntry.value;
        if (item is XmlNode && item.hasParent) {
          item = item.copy();
        }
        final itemXmlnsAttr = subXml.getXmlNamespaceAttribute() ?? keyXmlns;
        final newItemName =
            XmlName(subXml.xmlName ?? key.local, subXml.prefix ?? key.prefix);

        // non-wrapped lists and maps do not require returning an XML element,
        // and should be returned as is.
        if (item is XmlDocumentFragment) {
          if (!subXml.wrapped) {
            return item;
          }
        }
        return XmlElement(
          newItemName,
          [
            if (item == null)
              XmlSerializationContext.nilXmlAttribute.copy()
            else if (item is XmlAttribute)
              item,
            if (itemXmlnsAttr != null) itemXmlnsAttr,
          ],
          [if (item is XmlNode && item is! XmlAttribute) item],
        );
      default:
        return XmlElement(key);
    }
  }

  XmlNode handleAttributes(XmlReflection xml, XmlNode node) {
    if (xml.attribute) {
      if (node is XmlAttribute) {
        return node;
      }
      if (node case XmlHasName namedNode) {
        return XmlAttribute(
          namedNode.name.copy(),
          node.value ?? node.innerText,
        );
      } else {
        return node;
      }
    } else {
      return node;
    }
  }
}

import 'package:xml/xml.dart';
import 'package:openapi_infrastructure/openapi_infrastructure.dart';

extension UndefinedWrapperXmlNodeExtensions on XmlNode {
  Map<String?, XmlNode> readNormalized() {
    final srcMap = <String?, List<XmlNode>>{};
    for (final child in children.followedBy(attributes)) {
      String? srcMapKey;
      if (child case XmlHasName namedChild) {
        srcMapKey = namedChild.name.qualified;
      }
      final nodes = srcMap[srcMapKey] ??= [];
      nodes.add(child);
    }
    final srcNormalized = srcMap.map(
      (key, value) => MapEntry(
        key,
        value.normalize(),
      ),
    );
    return srcNormalized;
  }
  UndefinedWrapper<XmlElement> getElementOrUndefined(
    String key, {
    String? namespace,
  }) {
    final element = getElement(key, namespace: namespace);
    if (element == null) {
      return UndefinedWrapper.undefined();
    } else {
      return UndefinedWrapper(element);
    }
  }

  UndefinedWrapper<T> getElementOrUndefinedMapped<T>(
    String key,
    T Function(XmlElement src) mapper, {
    String? namespace,
  }) {
    return getElementOrUndefined(key, namespace: namespace).map(mapper);
  }

  T getElementRequiredMapped<T>(
    String key,
    T Function(XmlElement src) mapper, {
    String? namespace,
  }) {
    final element = getElement(key, namespace: namespace);
    if (element == null) {
      throw ArgumentError(
        'Element ($key) is not present in the Node, but it is required.',
        'key',
      );
    } else {
      return mapper(element);
    }
  }

  UndefinedWrapper<XmlAttribute> getAttributeOrUndefined(
    String key, {
    String? namespace,
  }) {
    final element = getAttributeNode(key, namespace: namespace);
    if (element == null) {
      return UndefinedWrapper.undefined();
    } else {
      return UndefinedWrapper(element);
    }
  }

  UndefinedWrapper<T> getAttributeOrUndefinedMapped<T>(
    String key,
    T Function(XmlAttribute src) mapper, {
    String? namespace,
  }) {
    return getAttributeOrUndefined(key, namespace: namespace).map(mapper);
  }

  T getAttributeRequiredMapped<T>(
    String key,
    T Function(XmlAttribute src) mapper, {
    String? namespace,
  }) {
    final element = getAttributeNode(key, namespace: namespace);
    if (element == null) {
      throw ArgumentError(
        'Element ($key) is not present in the Node, but it is required.',
        'key',
      );
    } else {
      return mapper(element);
    }
  }
}

extension XmlNodeListExtensions on Iterable<XmlNode> {
  XmlNode normalize() {
    final length = this.length;
    return length == 0
        ? XmlDocumentFragment([])
        : length == 1
            ? first
            : XmlDocumentFragment(this.map((e) => e.copy()));
  }
}

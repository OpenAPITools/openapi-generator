import 'package:xml/xml.dart';
import 'package:shared_infrastructure/shared_infrastructure.dart';

extension UndefinedWrapperXmlNodeExtensions on XmlNode {
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

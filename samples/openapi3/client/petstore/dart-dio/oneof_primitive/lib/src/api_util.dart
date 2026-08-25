//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'dart:typed_data';

import 'package:built_collection/built_collection.dart';
import 'package:built_value/serializer.dart';
import 'package:dio/dio.dart';

/// Format normal parameters for query/form bodies.
/// Returns primitive, String, or `ListParam` for collections when `asString`
/// is false.
dynamic encodeParameter<T>(
  Serializers serializers,
  dynamic value,
  FullType type,
  {
  ListFormat format = ListFormat.multi,
  bool asString = false,
  bool forMultipart = false,
}) {
  if (value == null) {
    return asString ? '' : null;
  }
  if (value is String || value is num || value is bool) {
    return value;
  }
  if (value is Uint8List) {
    return value;
  }
  final serialized = serializers.serialize(
    value as Object,
    specifiedType: type,
  );
  if (serialized == null) {
    return asString ? '' : null;
  }
  if (value is BuiltList<T> || value is BuiltSet<T>) {
    final values = List<Object?>.of((serialized as Iterable<Object?>).cast());
    if (asString && format != ListFormat.multi && format != ListFormat.multiCompatible) {
      return _joinCollectionValues(values, format);
    }
    if (forMultipart) {
      return serialized;
    }
    return ListParam(values, format);
  }
  return serialized;
}

String encodePathParameter(
  Serializers serializers,
  dynamic value,
  FullType type, {
  ListFormat format = ListFormat.multi,
}) {
  if (value == null) {
    return '';
  }
  final serialized = serializers.serialize(
    value as Object,
    specifiedType: type,
  );
  if (serialized == null) {
    return '';
  }
  if (serialized is String || serialized is num || serialized is bool) {
    return Uri.encodeComponent(serialized.toString());
  }
  if (serialized is List) {
    final values = serialized.map((item) => _encodePathValue(item, format)).toList();
    return _joinCollectionValues(values, format);
  }
  if (serialized is Map) {
    final pairs = <String>[];
    serialized.forEach((k, v) {
      final serializedKey = _encodePathValue(k, format);
      final serializedValue = _encodePathValue(v, format);
      pairs.add('$serializedKey,$serializedValue');
    });
    return pairs.join(',');
  }
  return Uri.encodeComponent(serialized.toString());
}

String _encodePathValue(dynamic value, ListFormat format) {
  if (value == null) {
    return '';
  }
  if (value is String) {
    return Uri.encodeComponent(value);
  }
  if (value is num || value is bool) {
    return Uri.encodeComponent(value.toString());
  }
  if (value is Enum) {
    return Uri.encodeComponent(value.name);
  }
  if (value is List) {
    final values = value.map((item) => _encodePathValue(item, format)).toList();
    return _joinCollectionValues(values, format);
  }
  if (value is Map) {
    final pairs = <String>[];
    value.forEach((k, v) {
      final serializedKey = _encodePathValue(k, format);
      final serializedValue = _encodePathValue(v, format);
      pairs.add('$serializedKey,$serializedValue');
    });
    return pairs.join(',');
  }
  return Uri.encodeComponent(value.toString());
}

String _joinCollectionValues(List<Object?> values, ListFormat format) {
  switch (format) {
    case ListFormat.csv:
      return values.join(',');
    case ListFormat.ssv:
      return values.join(' ');
    case ListFormat.tsv:
      return values.join('\t');
    case ListFormat.pipes:
      return values.join('|');
    case ListFormat.multi:
      return values.join(',');
    case ListFormat.multiCompatible:
      return values.join(',');
  }
}

void removeNullParametersExcept(
  Map<String, dynamic> parameters,
  Set<String> requiredParameters,
) {
  parameters.removeWhere((key, value) => value == null && !requiredParameters.contains(key));
}

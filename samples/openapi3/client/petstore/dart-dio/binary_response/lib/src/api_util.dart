//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:dio/dio.dart';

String encodePathParameter(
  dynamic value, {
  ListFormat format = ListFormat.multi,
}) {
  if (value == null) {
    return '';
  }
  if (value is String || value is num || value is bool) {
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

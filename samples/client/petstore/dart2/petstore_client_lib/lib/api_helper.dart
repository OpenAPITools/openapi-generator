//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element
// ignore_for_file: always_put_required_named_parameters_first

part of openapi.api;

// port from Java version
Iterable<QueryParam> _convertParametersForCollectionFormat(
  String collectionFormat,
  String name,
  dynamic value,
) {
  final params = <QueryParam>[];

  // preconditions
  if (name != null && !name.isEmpty && value != null) {
    if (value is List) {
      List values = value as List;

      // get the collection format, default: csv
      collectionFormat = (collectionFormat == null || collectionFormat.isEmpty)
        ? 'csv'
        : collectionFormat;

      if (collectionFormat == 'multi') {
        return values.map((v) => QueryParam(name, parameterToString(v)));
      }

      final delimiter = _delimiters[collectionFormat] ?? ',';

      params.add(QueryParam(name, values.map((v) => parameterToString(v)).join(delimiter)));
    } else {
      params.add(QueryParam(name, parameterToString(value)));
    }
  }

  return params;
}

/// Format the given parameter object into a [String].
String parameterToString(dynamic value) {
  if (value == null) {
    return '';
  }
  if (value is DateTime) {
    return value.toUtc().toIso8601String();
  }
  return value.toString();
}

/// Returns the decoded body as UTF-8 if the given headers indicate an 'application/json' content type.
/// Otherwise, returns the decoded body as decoded by dart:http package.
String _decodeBodyBytes(Response response) {
  final contentType = response.headers['content-type'];
  return contentType != null && contentType.contains('application/json')
    ? utf8.decode(response.bodyBytes)
    : response.body;
}

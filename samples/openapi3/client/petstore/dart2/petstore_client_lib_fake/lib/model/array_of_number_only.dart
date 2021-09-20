//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ArrayOfNumberOnly {
  /// Returns a new [ArrayOfNumberOnly] instance.
  ArrayOfNumberOnly({
    this.arrayNumber = const [],
  });

  List<num> arrayNumber;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ArrayOfNumberOnly &&
     other.arrayNumber == arrayNumber;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (arrayNumber == null ? 0 : arrayNumber.hashCode);

  @override
  String toString() => 'ArrayOfNumberOnly[arrayNumber=$arrayNumber]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (arrayNumber != null) {
      json[r'ArrayNumber'] = arrayNumber;
    }
    return json;
  }

  /// Returns a new [ArrayOfNumberOnly] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ArrayOfNumberOnly fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return ArrayOfNumberOnly(
        arrayNumber: json[r'ArrayNumber'] is List
          ? (json[r'ArrayNumber'] as List).cast<num>()
          : null,
      );
    }
    return null;
  }

  static List<ArrayOfNumberOnly> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(ArrayOfNumberOnly.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <ArrayOfNumberOnly>[];

  static Map<String, ArrayOfNumberOnly> mapFromJson(dynamic json) {
    final map = <String, ArrayOfNumberOnly>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = ArrayOfNumberOnly.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ArrayOfNumberOnly-objects as value to a dart map
  static Map<String, List<ArrayOfNumberOnly>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ArrayOfNumberOnly>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ArrayOfNumberOnly.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}


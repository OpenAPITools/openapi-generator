//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ArrayOfArrayOfNumberOnly {
  /// Returns a new [ArrayOfArrayOfNumberOnly] instance.
  ArrayOfArrayOfNumberOnly({
    this.arrayArrayNumber = const [],
  });

  List<List<num>> arrayArrayNumber;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ArrayOfArrayOfNumberOnly &&
     other.arrayArrayNumber == arrayArrayNumber;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (arrayArrayNumber == null ? 0 : arrayArrayNumber.hashCode);

  @override
  String toString() => 'ArrayOfArrayOfNumberOnly[arrayArrayNumber=$arrayArrayNumber]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (arrayArrayNumber != null) {
      json[r'ArrayArrayNumber'] = arrayArrayNumber;
    }
    return json;
  }

  /// Returns a new [ArrayOfArrayOfNumberOnly] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ArrayOfArrayOfNumberOnly fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return ArrayOfArrayOfNumberOnly(
        arrayArrayNumber: json[r'ArrayArrayNumber'] is List
          ? (json[r'ArrayArrayNumber'] as List).map(
              (e) => e == null ? null : (e as List).cast<num>()
            ).toList(growable: false)
          : null,
      );
    }
    return null;
  }

  static List<ArrayOfArrayOfNumberOnly> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(ArrayOfArrayOfNumberOnly.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <ArrayOfArrayOfNumberOnly>[];

  static Map<String, ArrayOfArrayOfNumberOnly> mapFromJson(dynamic json) {
    final map = <String, ArrayOfArrayOfNumberOnly>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = ArrayOfArrayOfNumberOnly.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ArrayOfArrayOfNumberOnly-objects as value to a dart map
  static Map<String, List<ArrayOfArrayOfNumberOnly>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ArrayOfArrayOfNumberOnly>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ArrayOfArrayOfNumberOnly.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}


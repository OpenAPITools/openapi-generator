//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
  /// [json] if it's non-null, null if [json] is null.
  static ArrayOfArrayOfNumberOnly fromJson(Map<String, dynamic> json) => json == null
    ? null
    : ArrayOfArrayOfNumberOnly(
        arrayArrayNumber: json[r'ArrayArrayNumber'] == null
          ? null
          : (json[r'ArrayArrayNumber'] as List).map(
              (e) => e == null ? null : (e as List).cast<num>()
            ).toList(growable: false),
    );

  static List<ArrayOfArrayOfNumberOnly> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ArrayOfArrayOfNumberOnly>[]
      : json.map((dynamic value) => ArrayOfArrayOfNumberOnly.fromJson(value)).toList(growable: true == growable);

  static Map<String, ArrayOfArrayOfNumberOnly> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ArrayOfArrayOfNumberOnly>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = ArrayOfArrayOfNumberOnly.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ArrayOfArrayOfNumberOnly-objects as value to a dart map
  static Map<String, List<ArrayOfArrayOfNumberOnly>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ArrayOfArrayOfNumberOnly>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = ArrayOfArrayOfNumberOnly.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}


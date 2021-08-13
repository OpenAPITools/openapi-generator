//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
  /// [json] if it's non-null, null if [json] is null.
  static ArrayOfNumberOnly fromJson(Map<String, dynamic> json) => json == null
    ? null
    : ArrayOfNumberOnly(
        arrayNumber: json[r'ArrayNumber'] == null
          ? null
          : (json[r'ArrayNumber'] as List).cast<num>(),
    );

  static List<ArrayOfNumberOnly> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ArrayOfNumberOnly>[]
      : json.map((dynamic value) => ArrayOfNumberOnly.fromJson(value)).toList(growable: true == growable);

  static Map<String, ArrayOfNumberOnly> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ArrayOfNumberOnly>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = ArrayOfNumberOnly.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ArrayOfNumberOnly-objects as value to a dart map
  static Map<String, List<ArrayOfNumberOnly>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ArrayOfNumberOnly>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = ArrayOfNumberOnly.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}


//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class DogAllOf {
  /// Returns a new [DogAllOf] instance.
  DogAllOf({
    this.breed,
  });

  String breed;

  @override
  bool operator ==(Object other) => identical(this, other) || other is DogAllOf &&
     other.breed == breed;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (breed == null ? 0 : breed.hashCode);

  @override
  String toString() => 'DogAllOf[breed=$breed]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (breed != null) {
      json[r'breed'] = breed;
    }
    return json;
  }

  /// Returns a new [DogAllOf] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static DogAllOf fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return DogAllOf(
        breed: mapValueOfType<String>(json, r'breed'),
      );
    }
    return null;
  }

  static List<DogAllOf> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(DogAllOf.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <DogAllOf>[];

  static Map<String, DogAllOf> mapFromJson(dynamic json) {
    final map = <String, DogAllOf>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = DogAllOf.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of DogAllOf-objects as value to a dart map
  static Map<String, List<DogAllOf>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<DogAllOf>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = DogAllOf.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}


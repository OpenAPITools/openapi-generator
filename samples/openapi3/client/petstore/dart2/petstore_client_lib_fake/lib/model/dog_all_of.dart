//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

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


  String? breed;

  @override
  bool operator ==(Object other) => identical(this, other) || other is DogAllOf &&
     other.breed == breed;

  @override
  int get hashCode =>
    breed.hashCode;

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
  static DogAllOf fromJson(Map<String, dynamic> json) => DogAllOf(
        breed: json[r'breed'] as String,
    );

  static List<DogAllOf> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<DogAllOf>((i) => DogAllOf.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <DogAllOf>[];

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
  static Map<String, List<DogAllOf>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<DogAllOf>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = DogAllOf.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}


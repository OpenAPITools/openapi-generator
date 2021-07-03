//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
  /// [json] if it's non-null, null if [json] is null.
  static DogAllOf fromJson(Map<String, dynamic> json) => json == null
    ? null
    : DogAllOf(
        breed: json[r'breed'],
    );

  static List<DogAllOf> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <DogAllOf>[]
      : json.map((dynamic value) => DogAllOf.fromJson(value)).toList(growable: true == growable);

  static Map<String, DogAllOf> mapFromJson(Map<String, dynamic> json) {
    final map = <String, DogAllOf>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = DogAllOf.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of DogAllOf-objects as value to a dart map
  static Map<String, List<DogAllOf>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<DogAllOf>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = DogAllOf.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}


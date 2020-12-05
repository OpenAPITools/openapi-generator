//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class UpdatePetWithFormBody {
  /// Returns a new [UpdatePetWithFormBody] instance.
  UpdatePetWithFormBody({
    this.name,
    this.status,
  });

  /// Updated name of the pet
  String name;

  /// Updated status of the pet
  String status;

  @override
  bool operator ==(Object other) => identical(this, other) || other is UpdatePetWithFormBody &&
     other.name == name &&
     other.status == status;

  @override
  int get hashCode =>
    (name == null ? 0 : name.hashCode) +
    (status == null ? 0 : status.hashCode);

  @override
  String toString() => 'UpdatePetWithFormBody[name=$name, status=$status]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (name != null) {
      json['name'] = name;
    }
    if (status != null) {
      json['status'] = status;
    }
    return json;
  }

  /// Returns a new [UpdatePetWithFormBody] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static UpdatePetWithFormBody fromJson(Map<String, dynamic> json) => json == null
    ? null
    : UpdatePetWithFormBody(
        name: json['name'],
        status: json['status'],
    );

  static List<UpdatePetWithFormBody> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <UpdatePetWithFormBody>[]
      : json.map((v) => UpdatePetWithFormBody.fromJson(v)).toList(growable: true == growable);

  static Map<String, UpdatePetWithFormBody> mapFromJson(Map<String, dynamic> json) {
    final map = <String, UpdatePetWithFormBody>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = UpdatePetWithFormBody.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of UpdatePetWithFormBody-objects as value to a dart map
  static Map<String, List<UpdatePetWithFormBody>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<UpdatePetWithFormBody>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = UpdatePetWithFormBody.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}


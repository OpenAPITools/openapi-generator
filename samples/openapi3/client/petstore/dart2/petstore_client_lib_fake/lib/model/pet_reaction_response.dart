//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class PetReactionResponse {
  /// Returns a new [PetReactionResponse] instance.
  PetReactionResponse({
    this.petId,
    this.status,
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  int? petId;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? status;

  @override
  bool operator ==(Object other) => identical(this, other) || other is PetReactionResponse &&
    other.petId == petId &&
      other.status == status;
  

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (petId == null ? 0 : petId!.hashCode) +
      (status == null ? 0 : status!.hashCode);
  

  @override
  String toString() => 'PetReactionResponse[petId=$petId, status=$status]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.petId != null) {
      json[r'petId'] = this.petId;
    } else {
      json[r'petId'] = null;
    }
    if (this.status != null) {
      json[r'status'] = this.status;
    } else {
      json[r'status'] = null;
    }
    return json;
  }

  /// Returns a new [PetReactionResponse] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static PetReactionResponse? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        return true;
      }());

      return PetReactionResponse(
        petId: mapValueOfType<int>(json, r'petId'),
        status: mapValueOfType<String>(json, r'status'),
      );
    }
    return null;
  }

  static List<PetReactionResponse> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <PetReactionResponse>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = PetReactionResponse.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, PetReactionResponse> mapFromJson(dynamic json) {
    final map = <String, PetReactionResponse>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = PetReactionResponse.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of PetReactionResponse-objects as value to a dart map
  static Map<String, List<PetReactionResponse>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<PetReactionResponse>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = PetReactionResponse.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}


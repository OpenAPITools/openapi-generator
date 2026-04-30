//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class PetReactionsResponse {
  /// Returns a new [PetReactionsResponse] instance.
  PetReactionsResponse({
    this.myReacts = const {},
    this.reactionCounts = const {},
  });

  Map<String, Map<String, bool>> myReacts;

  Map<String, Map<String, int>> reactionCounts;

  @override
  bool operator ==(Object other) => identical(this, other) || other is PetReactionsResponse &&
    _deepEquality.equals(other.myReacts, myReacts) &&
      _deepEquality.equals(other.reactionCounts, reactionCounts);
  

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (myReacts.hashCode) +
      (reactionCounts.hashCode);
  

  @override
  String toString() => 'PetReactionsResponse[myReacts=$myReacts, reactionCounts=$reactionCounts]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'myReacts'] = this.myReacts;
      json[r'reactionCounts'] = this.reactionCounts;
    return json;
  }

  /// Returns a new [PetReactionsResponse] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static PetReactionsResponse? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        return true;
      }());

      return PetReactionsResponse(
        myReacts: json[r'myReacts'] == null
            ? const {}
            : (json[r'myReacts'] as Map).map((k, v) => MapEntry(k as String, (v as Map).cast<String, bool>())),
        reactionCounts: json[r'reactionCounts'] == null
            ? const {}
            : (json[r'reactionCounts'] as Map).map((k, v) => MapEntry(k as String, (v as Map).cast<String, int>())),
      );
    }
    return null;
  }

  static List<PetReactionsResponse> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <PetReactionsResponse>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = PetReactionsResponse.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, PetReactionsResponse> mapFromJson(dynamic json) {
    final map = <String, PetReactionsResponse>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = PetReactionsResponse.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of PetReactionsResponse-objects as value to a dart map
  static Map<String, List<PetReactionsResponse>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<PetReactionsResponse>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = PetReactionsResponse.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}


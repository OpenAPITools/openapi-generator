//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ModelReturn {
  /// Returns a new [ModelReturn] instance.
  ModelReturn({
    this.return_,
  });

  int return_;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelReturn &&
     other.return_ == return_;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (return_.hashCode);

  @override
  String toString() => 'ModelReturn[return_=$return_]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'return'] = return_;
    return json;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    
  };

  /// Returns a new [ModelReturn] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ModelReturn? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(
        false,
        () {
          for (final key in requiredKeys) {
            if (!json.containsKey(key)) {
              throw FormatException('Required key "ModelReturn.$key" is missing from JSON.', json);
            }
            final value = json[key];
            if (null == value) {
              throw FormatException('Required key "ModelReturn.$key" cannot be null.', json);
            }
          }
        },
      );

      return ModelReturn(
        return_: mapValueOfType<int>(json, r'return'),
      );
    }
    return null;
  }

  static List<ModelReturn>? listFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final result = <ModelReturn>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ModelReturn.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return emptyIsNull ? null : result.toList(growable: growable);
  }

  static Map<String, ModelReturn> mapFromJson(dynamic json) {
    final map = <String, ModelReturn>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ModelReturn.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ModelReturn-objects as value to a dart map
  static Map<String, List<ModelReturn>> mapListFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final map = <String, List<ModelReturn>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ModelReturn.listFromJson(entry.value, emptyIsNull: emptyIsNull, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }
}


//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

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
    (return_ == null ? 0 : return_.hashCode);

  @override
  String toString() => 'ModelReturn[return_=$return_]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (return_ != null) {
      json[r'return'] = return_;
    }
    return json;
  }

  /// Returns a new [ModelReturn] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ModelReturn fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return ModelReturn(
        return_: mapValueOfType<int>(json, r'return'),
      );
    }
    return null;
  }

  static List<ModelReturn> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(ModelReturn.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <ModelReturn>[];

  static Map<String, ModelReturn> mapFromJson(dynamic json) {
    final map = <String, ModelReturn>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = ModelReturn.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ModelReturn-objects as value to a dart map
  static Map<String, List<ModelReturn>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ModelReturn>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ModelReturn.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}


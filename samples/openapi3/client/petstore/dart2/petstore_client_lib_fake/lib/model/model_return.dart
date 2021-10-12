//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

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


  int? return_;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelReturn &&
     other.return_ == return_;

  @override
  int get hashCode =>
    return_.hashCode;

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
  static ModelReturn fromJson(Map<String, dynamic> json) => ModelReturn(
        return_: json[r'return'] as int,
    );

  static List<ModelReturn> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<ModelReturn>((i) => ModelReturn.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <ModelReturn>[];

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
  static Map<String, List<ModelReturn>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<ModelReturn>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ModelReturn.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}


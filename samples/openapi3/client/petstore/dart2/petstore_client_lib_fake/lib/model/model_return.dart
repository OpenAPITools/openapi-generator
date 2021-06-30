//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
  /// [json] if it's non-null, null if [json] is null.
  static ModelReturn fromJson(Map<String, dynamic> json) => json == null
    ? null
    : ModelReturn(
        return_: json[r'return'],
    );

  static List<ModelReturn> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ModelReturn>[]
      : json.map((dynamic value) => ModelReturn.fromJson(value)).toList(growable: true == growable);

  static Map<String, ModelReturn> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ModelReturn>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = ModelReturn.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ModelReturn-objects as value to a dart map
  static Map<String, List<ModelReturn>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ModelReturn>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = ModelReturn.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}


//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Model200Response {
  /// Returns a new [Model200Response] instance.
  Model200Response({
    this.name,
    this.class_,
  });

  int name;

  String class_;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Model200Response &&
     other.name == name &&
     other.class_ == class_;

  @override
  int get hashCode =>
    (name == null ? 0 : name.hashCode) +
    (class_ == null ? 0 : class_.hashCode);

  @override
  String toString() => 'Model200Response[name=$name, class_=$class_]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (name != null) {
      json[r'name'] = name;
    }
    if (class_ != null) {
      json[r'class'] = class_;
    }
    return json;
  }

  /// Returns a new [Model200Response] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static Model200Response fromJson(Map<String, dynamic> json) => json == null
    ? null
    : Model200Response(
        name: json[r'name'],
        class_: json[r'class'],
    );

  static List<Model200Response> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Model200Response>[]
      : json.map((dynamic value) => Model200Response.fromJson(value)).toList(growable: true == growable);

  static Map<String, Model200Response> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Model200Response>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = Model200Response.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Model200Response-objects as value to a dart map
  static Map<String, List<Model200Response>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Model200Response>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = Model200Response.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}


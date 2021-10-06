//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Model200Response {
  /// Returns a new [Model200Response] instance.
  Model200Response({
    this.name,
    this.class_,
  });


  int? name;

  String? class_;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Model200Response &&
     other.name == name &&
     other.class_ == class_;

  @override
  int get hashCode =>
    name.hashCode +
    class_.hashCode;

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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Model200Response fromJson(Map<String, dynamic> json) => Model200Response(
        name: json[r'name'] as int,
        class_: json[r'class'] as String,
    );

  static List<Model200Response> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<Model200Response>((i) => Model200Response.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <Model200Response>[];

  static Map<String, Model200Response> mapFromJson(dynamic json) {
    final map = <String, Model200Response>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = Model200Response.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Model200Response-objects as value to a dart map
  static Map<String, List<Model200Response>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<Model200Response>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Model200Response.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}


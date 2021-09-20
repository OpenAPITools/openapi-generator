//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

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

  int name;

  String class_;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Model200Response &&
     other.name == name &&
     other.class_ == class_;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Model200Response fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return Model200Response(
        name: mapValueOfType<int>(json, r'name'),
        class_: mapValueOfType<String>(json, r'class'),
      );
    }
    return null;
  }

  static List<Model200Response> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(Model200Response.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <Model200Response>[];

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
  static Map<String, List<Model200Response>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Model200Response>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Model200Response.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}


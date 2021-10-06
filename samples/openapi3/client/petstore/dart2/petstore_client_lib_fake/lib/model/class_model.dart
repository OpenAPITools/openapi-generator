//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ClassModel {
  /// Returns a new [ClassModel] instance.
  ClassModel({
    this.class_,
  });


  String? class_;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ClassModel &&
     other.class_ == class_;

  @override
  int get hashCode =>
    class_.hashCode;

  @override
  String toString() => 'ClassModel[class_=$class_]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (class_ != null) {
      json[r'_class'] = class_;
    }
    return json;
  }

  /// Returns a new [ClassModel] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ClassModel fromJson(Map<String, dynamic> json) => ClassModel(
        class_: json[r'_class'] as String,
    );

  static List<ClassModel> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<ClassModel>((i) => ClassModel.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <ClassModel>[];

  static Map<String, ClassModel> mapFromJson(dynamic json) {
    final map = <String, ClassModel>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = ClassModel.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ClassModel-objects as value to a dart map
  static Map<String, List<ClassModel>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<ClassModel>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ClassModel.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}


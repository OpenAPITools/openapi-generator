//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ClassModel {
  /// Returns a new [ClassModel] instance.
  ClassModel({
    this.class_,
  });

  /// Returns a new [ClassModel] instance and optionally import its values from
  /// [json] if it's non-null.
  ClassModel.fromJson(Map<String, dynamic> json) {
    if (json != null) {
      class_ = json['_class'];
    }
  }

  
  String class_;

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
      json['_class'] = class_;
    }
    return json;
  }

  static List<ClassModel> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ClassModel>[]
      : json.map((v) => ClassModel.fromJson(v)).toList(growable: true == growable);

  static Map<String, ClassModel> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ClassModel>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = ClassModel.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of ClassModel-objects as value to a dart map
  static Map<String, List<ClassModel>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ClassModel>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = ClassModel.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}


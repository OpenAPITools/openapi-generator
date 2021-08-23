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

  String class_;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ClassModel &&
     other.class_ == class_;

  @override
  int get hashCode =>
    (class_ == null ? 0 : class_.hashCode);

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
  /// [json] if it's non-null, null if [json] is null.
  static ClassModel fromJson(Map<String, dynamic> json) => json == null
    ? null
    : ClassModel(
        class_: json[r'_class'],
    );

  static List<ClassModel> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ClassModel>[]
      : json.map((dynamic value) => ClassModel.fromJson(value)).toList(growable: true == growable);

  static Map<String, ClassModel> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ClassModel>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = ClassModel.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ClassModel-objects as value to a dart map
  static Map<String, List<ClassModel>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ClassModel>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = ClassModel.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}


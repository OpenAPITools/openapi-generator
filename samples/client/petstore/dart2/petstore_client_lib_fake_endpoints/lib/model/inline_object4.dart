//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class InlineObject4 {
  /// Returns a new [InlineObject4] instance.
  InlineObject4({
    @required this.param,
    @required this.param2,
  });

  /// field1
  String param;

  /// field2
  String param2;

  @override
  bool operator ==(Object other) => identical(this, other) || other is InlineObject4 &&
     other.param == param &&
     other.param2 == param2;

  @override
  int get hashCode =>
    (param == null ? 0 : param.hashCode) +
    (param2 == null ? 0 : param2.hashCode);

  @override
  String toString() => 'InlineObject4[param=$param, param2=$param2]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (param != null) {
      json['param'] = param;
    }
    if (param2 != null) {
      json['param2'] = param2;
    }
    return json;
  }

  /// Returns a new [InlineObject4] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static InlineObject4 fromJson(Map<String, dynamic> json) => json == null
    ? null
    : InlineObject4(
        param: json['param'],
        param2: json['param2'],
    );

  static List<InlineObject4> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <InlineObject4>[]
      : json.map((v) => InlineObject4.fromJson(v)).toList(growable: true == growable);

  static Map<String, InlineObject4> mapFromJson(Map<String, dynamic> json) {
    final map = <String, InlineObject4>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = InlineObject4.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of InlineObject4-objects as value to a dart map
  static Map<String, List<InlineObject4>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<InlineObject4>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = InlineObject4.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}


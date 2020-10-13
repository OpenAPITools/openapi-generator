//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class List {
  /// Returns a new [List] instance.
  List({
    this.n123list,
  });

  /// Returns a new [List] instance and optionally import its values from
  /// [json] if it's non-null.
  List.fromJson(Map<String, dynamic> json) {
    if (json != null) {
      n123list = json['123-list'];
    }
  }

  
  String n123list;

  @override
  bool operator ==(Object other) => identical(this, other) || other is List &&
     other.n123list == n123list;

  @override
  int get hashCode =>
    n123list.hashCode;

  @override
  String toString() => 'List[n123list=$n123list]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (n123list != null) {
      json['123-list'] = n123list;
    }
    return json;
  }

  static List<List> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <List>[]
      : json.map((v) => List.fromJson(v)).toList(growable: true == growable);

  static Map<String, List> mapFromJson(Map<String, dynamic> json) {
    final map = <String, List>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = List.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of List-objects as value to a dart map
  static Map<String, List<List>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<List>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = List.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}


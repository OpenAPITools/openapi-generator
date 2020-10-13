//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class InlineObject5 {
  /// Returns a new [InlineObject5] instance.
  InlineObject5({
    this.additionalMetadata,
    @required this.requiredFile,
  });

  /// Returns a new [InlineObject5] instance and optionally import its values from
  /// [json] if it's non-null.
  InlineObject5.fromJson(Map<String, dynamic> json) {
    if (json != null) {
      additionalMetadata = json['additionalMetadata'];
      requiredFile = File.fromJson(json['requiredFile']);
    }
  }

  /// Additional data to pass to server
  String additionalMetadata;

  /// file to upload
  MultipartFile requiredFile;

  @override
  bool operator ==(Object other) => identical(this, other) || other is InlineObject5 &&
     other.additionalMetadata == additionalMetadata &&
     other.requiredFile == requiredFile;

  @override
  int get hashCode =>
    additionalMetadata.hashCode +
    requiredFile.hashCode;

  @override
  String toString() => 'InlineObject5[additionalMetadata=$additionalMetadata, requiredFile=$requiredFile]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (additionalMetadata != null) {
      json['additionalMetadata'] = additionalMetadata;
    }
    if (requiredFile != null) {
      json['requiredFile'] = requiredFile;
    }
    return json;
  }

  static List<InlineObject5> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <InlineObject5>[]
      : json.map((v) => InlineObject5.fromJson(v)).toList(growable: true == growable);

  static Map<String, InlineObject5> mapFromJson(Map<String, dynamic> json) {
    final map = <String, InlineObject5>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = InlineObject5.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of InlineObject5-objects as value to a dart map
  static Map<String, List<InlineObject5>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<InlineObject5>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = InlineObject5.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}


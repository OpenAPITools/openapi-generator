//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class InlineObject1 {
  /// Returns a new [InlineObject1] instance.
  InlineObject1({
    this.additionalMetadata,
    this.file,
  });

  /// Additional data to pass to server
  String additionalMetadata;

  /// file to upload
  MultipartFile file;

  @override
  bool operator ==(Object other) => identical(this, other) || other is InlineObject1 &&
     other.additionalMetadata == additionalMetadata &&
     other.file == file;

  @override
  int get hashCode =>
    (additionalMetadata == null ? 0 : additionalMetadata.hashCode) +
    (file == null ? 0 : file.hashCode);

  @override
  String toString() => 'InlineObject1[additionalMetadata=$additionalMetadata, file=$file]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (additionalMetadata != null) {
      json['additionalMetadata'] = additionalMetadata;
    }
    if (file != null) {
      json['file'] = file;
    }
    return json;
  }

  /// Returns a new [InlineObject1] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static InlineObject1 fromJson(Map<String, dynamic> json) => json == null
    ? null
    : InlineObject1(
        additionalMetadata: json['additionalMetadata'],
        file: null, // No support for decoding binary content from JSON
    );

  static List<InlineObject1> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <InlineObject1>[]
      : json.map((v) => InlineObject1.fromJson(v)).toList(growable: true == growable);

  static Map<String, InlineObject1> mapFromJson(Map<String, dynamic> json) {
    final map = <String, InlineObject1>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = InlineObject1.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of InlineObject1-objects as value to a dart map
  static Map<String, List<InlineObject1>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<InlineObject1>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = InlineObject1.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}


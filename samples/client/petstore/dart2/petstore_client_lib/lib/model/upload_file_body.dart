//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class UploadFileBody {
  /// Returns a new [UploadFileBody] instance.
  UploadFileBody({
    this.additionalMetadata,
    this.file,
  });

  /// Additional data to pass to server
  String additionalMetadata;

  /// file to upload
  MultipartFile file;

  @override
  bool operator ==(Object other) => identical(this, other) || other is UploadFileBody &&
     other.additionalMetadata == additionalMetadata &&
     other.file == file;

  @override
  int get hashCode =>
    (additionalMetadata == null ? 0 : additionalMetadata.hashCode) +
    (file == null ? 0 : file.hashCode);

  @override
  String toString() => 'UploadFileBody[additionalMetadata=$additionalMetadata, file=$file]';

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

  /// Returns a new [UploadFileBody] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static UploadFileBody fromJson(Map<String, dynamic> json) => json == null
    ? null
    : UploadFileBody(
        additionalMetadata: json['additionalMetadata'],
        file: File.fromJson(json['file']),
    );

  static List<UploadFileBody> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <UploadFileBody>[]
      : json.map((v) => UploadFileBody.fromJson(v)).toList(growable: true == growable);

  static Map<String, UploadFileBody> mapFromJson(Map<String, dynamic> json) {
    final map = <String, UploadFileBody>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = UploadFileBody.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of UploadFileBody-objects as value to a dart map
  static Map<String, List<UploadFileBody>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<UploadFileBody>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = UploadFileBody.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}


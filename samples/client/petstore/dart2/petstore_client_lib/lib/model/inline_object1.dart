part of openapi.api;

class InlineObject1 {
  /// Additional data to pass to server
  String additionalMetadata;
  /// file to upload
  MultipartFile file;

  InlineObject1({
    this.additionalMetadata,
    this.file,
  });

  @override
  String toString() {
    return 'InlineObject1[additionalMetadata=$additionalMetadata, file=$file, ]';
  }

  InlineObject1.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    additionalMetadata = json['additionalMetadata'];
    file = (json['file'] == null) ?
      null :
      File.fromJson(json['file']);
  }

  Map<String, dynamic> toJson() {
    Map<String, dynamic> json = {};
    if (additionalMetadata != null)
      json['additionalMetadata'] = additionalMetadata;
    if (file != null)
      json['file'] = file;
    return json;
  }

  static List<InlineObject1> listFromJson(List<dynamic> json) {
    return json == null ? List<InlineObject1>() : json.map((value) => InlineObject1.fromJson(value)).toList();
  }

  static Map<String, InlineObject1> mapFromJson(Map<String, dynamic> json) {
    final map = Map<String, InlineObject1>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) => map[key] = InlineObject1.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of InlineObject1-objects as value to a dart map
  static Map<String, List<InlineObject1>> mapListFromJson(Map<String, dynamic> json) {
    final map = Map<String, List<InlineObject1>>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) {
        map[key] = InlineObject1.listFromJson(value);
      });
    }
    return map;
  }
}


part of openapi.api;

class InlineObject1 {
  /* Additional data to pass to server */
    String additionalMetadata = null;
  /* file to upload */
    MultipartFile file = null;
  InlineObject1();

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
    Map <String, dynamic> json = {};
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
    var map = Map<String, InlineObject1>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) => map[key] = InlineObject1.fromJson(value));
    }
    return map;
  }
}


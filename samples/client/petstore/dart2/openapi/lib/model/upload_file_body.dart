part of openapi.api;

class UploadFileBody {
  /* Additional data to pass to server */
  String additionalMetadata = null;
  /* file to upload */
  MultipartFile file = null;
  UploadFileBody();

  @override
  String toString() {
    return 'UploadFileBody[additionalMetadata=$additionalMetadata, file=$file, ]';
  }

  UploadFileBody.fromJson(Map<String, dynamic> json) {
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

  static List<UploadFileBody> listFromJson(List<dynamic> json) {
    return json == null ? List<UploadFileBody>() : json.map((value) => UploadFileBody.fromJson(value)).toList();
  }

  static Map<String, UploadFileBody> mapFromJson(Map<String, dynamic> json) {
    var map = Map<String, UploadFileBody>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) => map[key] = UploadFileBody.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of UploadFileBody-objects as value to a dart map
  static Map<String, List<UploadFileBody>> mapListFromJson(Map<String, dynamic> json) {
    var map = Map<String, List<UploadFileBody>>();
     if (json != null && json.isNotEmpty) {
       json.forEach((String key, dynamic value) {
         map[key] = UploadFileBody.listFromJson(value);
       });
     }
     return map;
  }
}


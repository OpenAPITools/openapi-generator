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
    if (json['additionalMetadata'] == null) {
      additionalMetadata = null;
    } else {
      additionalMetadata = json['additionalMetadata'];
    }
    if (json['file'] == null) {
      file = null;
    } else {
      file = new File.fromJson(json['file']);
    }
  }

  Map<String, dynamic> toJson() {
    return {
      'additionalMetadata': additionalMetadata,
      'file': file
    };
  }

  static List<UploadFileBody> listFromJson(List<dynamic> json) {
    return json == null ? new List<UploadFileBody>() : json.map((value) => new UploadFileBody.fromJson(value)).toList();
  }

  static Map<String, UploadFileBody> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, UploadFileBody>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new UploadFileBody.fromJson(value));
    }
    return map;
  }
}


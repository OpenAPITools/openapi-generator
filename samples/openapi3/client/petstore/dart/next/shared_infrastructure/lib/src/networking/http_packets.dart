
/// An http packet is:
/// 1. Headers
/// 2. Body
abstract class HttpPacketBase {
  const HttpPacketBase();
  Map<String, String> get headers;
  Stream<List<int>> get bodyBytesStream;
  Map<String, dynamic> get context;
}

extension HttpPacketBaseExtensions on HttpPacketBase {
  String get contentTypeOrDefault => contentType ?? 'application/octet-stream';
  String? get contentType => headers['Content-Type'];
  String? get normalizedMimeType =>
      contentType?.split(';').first.trim().toLowerCase();
  String? get normalizedCharset {
    final contentType = this.contentType;
    if (contentType == null) {
      return null;
    }
    var match = RegExp(r'charset=([^;]+)').firstMatch(contentType);
    if (match != null) {
      final charset = match.group(1);
      return charset;
    }
    //try getting the default charset from the mime type
    final mimeType = contentType.split(';').first.trim();
    if (mimeType == 'application/json') {
      return 'utf-8';
    } else if (mimeType.startsWith('text/')) {
      return 'us-ascii';
    }
    switch (mimeType) {
      case 'application/json':
        return 'utf-8';
      default:
        return null;
    }
  }
}

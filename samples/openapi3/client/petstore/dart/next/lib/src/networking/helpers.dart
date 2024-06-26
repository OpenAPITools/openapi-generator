class OASNetworkingUtils {
  static bool matchesStatusCodePattern(int statusCode, String pattern) {
    return false;
  }

  static bool matchesMimeTypePattern(String? contentType, String pattern) {
    return false;
  }


  static Stream<Uint8List>? encodeToBytes(Object? value, MediaType mimeType) {

  }
  static bool canEncodeToBytes(Object? value, MediaType mimeType) {
    switch (value) {
      case null:
        //a null value is equal to an empty stream.
        return true;
      case
    }
  }
  static Stream<Uint8List> _doEncodeToBytes(Object? value, MediaType mimeType) async* {

  }
}
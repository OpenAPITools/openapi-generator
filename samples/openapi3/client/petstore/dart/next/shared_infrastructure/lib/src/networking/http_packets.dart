///A meta packet describes just headers.
mixin HttpMetaPacketMixin {
  Map<String, String> get headers;
  Map<String, dynamic> get context;

  int? get contentLength {
    final contentLenHeader = headers['Content-Length'];
    if (contentLenHeader == null) {
      return null;
    }
    return int.tryParse(contentLenHeader);
  }
}

/// An http packet is:
/// 1. Headers
/// 2. Body
///
/// You can create an arbitrary packet from [HttpPacketMixin.memory] or
/// [HttpPacketMixin.stream] static methods.
mixin HttpPacketMixin on HttpMetaPacketMixin {
  Stream<List<int>> get bodyBytesStream;

  static MemoryHttpPacket memory({
    required List<int> bodyBytes,
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) {
    return MemoryHttpPacket(
      bodyBytes: bodyBytes,
      headers: headers,
      context: context,
    );
  }

  static StreamHttpPacket stream({
    int? contentLength,
    required Stream<List<int>> bodyBytesStream,
    required Map<String, String> headers,
    Map<String, dynamic> context = const {},
  }) {
    return StreamHttpPacket(
      bodyBytesStream: bodyBytesStream,
      headers: headers,
      context: context,
    );
  }
}

class MemoryHttpPacket with HttpMetaPacketMixin, HttpPacketMixin {
  final List<int> bodyBytes;
  @override
  final Map<String, dynamic> context;
  @override
  final Map<String, String> headers;

  const MemoryHttpPacket({
    required this.bodyBytes,
    required this.headers,
    this.context = const {},
  });

  @override
  Stream<List<int>> get bodyBytesStream => Stream.value(bodyBytes);

  @override
  int? get contentLength => bodyBytes.length;
}

class StreamHttpPacket with HttpMetaPacketMixin, HttpPacketMixin {
  @override
  final Map<String, dynamic> context;
  @override
  final Map<String, String> headers;

  final int? _contentLength;
  @override
  int? get contentLength => _contentLength ?? super.contentLength;

  const StreamHttpPacket({
    int? contentLength,
    required this.bodyBytesStream,
    required this.context,
    required this.headers,
  }) : _contentLength = contentLength;

  @override
  final Stream<List<int>> bodyBytesStream;
}

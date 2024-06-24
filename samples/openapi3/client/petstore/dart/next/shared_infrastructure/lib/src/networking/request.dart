import 'dart:convert';
import 'dart:math';
import 'dart:typed_data';
import 'package:http_parser/http_parser.dart';
import 'http_packets.dart';

/// A request, is an http packet directed to a url.
abstract base class HttpRequestBase with HttpMetaPacketMixin, HttpPacketMixin {
  const HttpRequestBase();

  static const getMethod = 'GET';
  static const postMethod = 'POST';
  static const putMethod = 'PUT';
  static const patchMethod = 'PATCH';
  static const deleteMethod = 'DELETE';
  static const headMethod = 'HEAD';

  Uri get url;
  String get method;

  const factory HttpRequestBase.memory({
    required Uri url,
    required String method,
    required Uint8List bodyBytes,
    required Map<String, String> headers,
    Map<String, dynamic> context,
  }) = MemoryHttpRequest;

  const factory HttpRequestBase.stream({
    int? contentLength,
    required Uri url,
    required String method,
    required Stream<List<int>> bodyBytesStream,
    required Map<String, String> headers,
    Map<String, dynamic> context,
  }) = StreamHttpRequest;
}

base class StreamHttpRequest extends HttpRequestBase implements StreamHttpPacket {
  @override
  final Stream<List<int>> bodyBytesStream;

  @override
  final Map<String, dynamic> context;

  @override
  final Map<String, String> headers;

  final int? _contentLength;
  @override
  int? get contentLength => _contentLength ?? super.contentLength;

  @override
  final Uri url;
  @override
  final String method;

  const StreamHttpRequest({
    int? contentLength,
    required this.bodyBytesStream,
    required this.url,
    required this.method,
    required this.headers,
    this.context = const {},
  }) : _contentLength = contentLength;
}

base class MemoryHttpRequest extends HttpRequestBase implements MemoryHttpPacket {
  @override
  final Uint8List bodyBytes;

  const MemoryHttpRequest({
    required this.url,
    required this.bodyBytes,
    required this.method,
    required this.headers,
    this.context = const {},
  });

  @override
  Stream<List<int>> get bodyBytesStream => Stream.value(bodyBytes);

  @override
  final Map<String, String> headers;

  @override
  final Uri url;
  @override
  final String method;

  @override
  final Map<String, dynamic> context;

  @override
  int? get contentLength => bodyBytes.length;
}

base class MultiPartHttpRequest extends HttpRequestBase {
  MultiPartHttpRequest({
    required Map<String, String> headers,
    required this.parts,
    required this.method,
    required this.url,
    this.context = const {},
  }) : _originalHeaders = headers;

  @override
  final Map<String, dynamic> context;

  @override
  final String method;

  @override
  final Uri url;

  final Map<String, String> _originalHeaders;
  CaseInsensitiveMap<String>? _processedHeaders;
  MediaType? _newContentType;
  CaseInsensitiveMap<String> _processHeaders() {
    final res = CaseInsensitiveMap.from(_originalHeaders);
    final originalContentType = res[_kContentType] ?? 'multipart/mixed';
    final originalContentTypeParsed = MediaType.parse(originalContentType);

    final newContentType = _newContentType =
        MediaType.parse(originalContentType).change(parameters: {
      ...originalContentTypeParsed.parameters,
      _kBoundary: _boundaryString(),
    });

    res[_kContentType] = newContentType.toString();

    return res;
  }

  @override
  Map<String, String> get headers {
    return _processedHeaders ?? _processHeaders();
  }

  final List<HttpPacketMixin> parts;

  @override
  Stream<List<int>> get bodyBytesStream {
    //process headers
    final _ = headers;
    //get result content type.
    final contentType = _newContentType!;

    final boundary = contentType.parameters[_kBoundary]!;
    return _finalize(boundary);
  }

  @override
  int? get contentLength {
    //super.contentLength will also call _processHeaders
    final superContentLength = super.contentLength;
    if (superContentLength != null) {
      //user wants to override content length via headers.
      return superContentLength;
    }

    var length = 0;
    for (var part in parts) {
      final partLen = part.contentLength;
      if (partLen == null) {
        return null;
      }
      length += '--'.length +
          _boundaryLength +
          '\r\n'.length +
          utf8.encode(_headerForPart(part)).length +
          partLen +
          '\r\n'.length;
    }

    return length + '--'.length + _boundaryLength + '--\r\n'.length;
  }

  /// The total length of the multipart boundaries used when building the
  /// request body.
  ///
  /// According to http://tools.ietf.org/html/rfc1341.html, this can't be longer
  /// than 70.
  static const int _boundaryLength = 70;
  static const _kBoundary = 'boundary';
  static const _kContentType = 'content-type';

  static final Random _random = Random();

  Stream<List<int>> _finalize(String boundary) async* {
    const line = [13, 10]; // \r\n
    final separator = utf8.encode('--$boundary\r\n');
    final close = utf8.encode('--$boundary--\r\n');

    for (final part in parts) {
      yield separator;
      yield utf8.encode(_headerForPart(part));
      yield* part.bodyBytesStream;
      yield line;
    }
    yield close;
  }

  /// Returns the header string for a part.
  ///
  /// The return value is guaranteed to contain only ASCII characters.
  String _headerForPart(HttpPacketMixin part) {
    var header =
        part.headers.entries.map((e) => '${e.key}; ${e.value}').join('\r\n');
    return '$header\r\n\r\n';
  }

  /// Returns a randomly-generated multipart boundary string
  String _boundaryString() {
    var prefix = 'dart-http-boundary-';
    var list = List<int>.generate(
        _boundaryLength - prefix.length,
        (index) =>
            _boundaryCharacters[_random.nextInt(_boundaryCharacters.length)],
        growable: false);
    return '$prefix${String.fromCharCodes(list)}';
  }
}

/// All character codes that are valid in multipart boundaries.
///
/// This is the intersection of the characters allowed in the `bcharsnospace`
/// production defined in [RFC 2046][] and those allowed in the `token`
/// production defined in [RFC 1521][].
///
/// [RFC 2046]: http://tools.ietf.org/html/rfc2046#section-5.1.1.
/// [RFC 1521]: https://tools.ietf.org/html/rfc1521#section-4
const List<int> _boundaryCharacters = <int>[
  43,
  95,
  45,
  46,
  48,
  49,
  50,
  51,
  52,
  53,
  54,
  55,
  56,
  57,
  65,
  66,
  67,
  68,
  69,
  70,
  71,
  72,
  73,
  74,
  75,
  76,
  77,
  78,
  79,
  80,
  81,
  82,
  83,
  84,
  85,
  86,
  87,
  88,
  89,
  90,
  97,
  98,
  99,
  100,
  101,
  102,
  103,
  104,
  105,
  106,
  107,
  108,
  109,
  110,
  111,
  112,
  113,
  114,
  115,
  116,
  117,
  118,
  119,
  120,
  121,
  122
];

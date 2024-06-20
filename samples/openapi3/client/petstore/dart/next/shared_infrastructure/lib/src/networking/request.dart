import 'dart:typed_data';

import 'http_packets.dart';

/// A request, is an http packet directed to a url.
abstract class HttpRequestBase extends HttpPacketBase {
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
    Map<String, String> headers,
    Map<String, dynamic> context,
  }) = MemoryHttpRequest;

  const factory HttpRequestBase.stream({
    required Uri url,
    required String method,
    required Stream<List<int>> bodyBytesStream,
    Map<String, String> headers,
    Map<String, dynamic> context,
  }) = StreamHttpRequest;
}

class StreamHttpRequest extends HttpRequestBase {
  @override
  final Stream<List<int>> bodyBytesStream;

  @override
  final Map<String, dynamic> context;

  @override
  final Map<String, String> headers;

  @override
  final Uri url;
  @override
  final String method;

  const StreamHttpRequest({
    required this.bodyBytesStream,
    required this.url,
    required this.method,
    this.context = const {},
    this.headers = const {},
  });
}

class MemoryHttpRequest extends HttpRequestBase {
  final Uint8List bodyBytes;

  const MemoryHttpRequest({
    required this.url,
    required this.bodyBytes,
    required this.method,
    this.headers = const {},
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
}

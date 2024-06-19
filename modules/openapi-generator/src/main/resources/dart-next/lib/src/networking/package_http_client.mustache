import 'package:shared_infrastructure/shared_infrastructure.dart';
import 'package:http/http.dart' as http;

class _BaseRequestWrapper extends http.BaseRequest {
  _BaseRequestWrapper(this.request) : super(request.method, request.url) {
    for (var element in request.headers.entries) {
      headers[element.key] = element.value;
    }
    if (request is MemoryHttpRequest) {
      contentLength = (request as MemoryHttpRequest).bodyBytes.length;
    }
  }

  final HttpRequestBase request;

  @override
  http.ByteStream finalize() {
    super.finalize();
    return http.ByteStream(request.bodyBytesStream);
  }
}

class PackageHttpNetworkingClient extends NetworkingClientBase {
  final http.Client client;

  PackageHttpNetworkingClient({
    http.Client? client,
  }) : client = client ?? http.Client();

  @override
  Future<HttpResponseBase> sendRequest(HttpRequestBase request) async {
    final mappedRequest = _BaseRequestWrapper(request);
    final response = await client.send(mappedRequest);

    return HttpResponseBase.stream(
      originalRequest: request,
      context: request.context,
      headers: request.headers,
      bodyBytesStream: response.stream,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
    );
  }
}

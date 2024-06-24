import 'request.dart';
import 'response.dart';

/// The networking client is only responsible to send a request (raw bytes) and
/// receive a response (raw bytes).
abstract class NetworkingClientBase {
  const NetworkingClientBase();

  /// Send an HttpRequest and get an HttpResponse
  Future<HttpResponseBase> sendRequest(HttpRequestBase request);
}

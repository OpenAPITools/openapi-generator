import 'package:petstore_api/_internal.dart';

part 'values_api.requests.dart';
part 'values_api.responses.dart';

class ValuesApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> userContext;

  const ValuesApi({
    required this.networkingClient,
    required this.baseUrl,
    this.userContext = const {},
  });

  Future<ValuesApiGetSomeValuesResponse> getSomeValues(
    ValuesApiGetSomeValuesRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return ValuesApiGetSomeValuesResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
}

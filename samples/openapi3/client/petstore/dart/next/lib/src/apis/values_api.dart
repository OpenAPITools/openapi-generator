import 'package:petstore_api/_internal.dart';

part 'values_api.requests.dart';
part 'values_api.responses.dart';

class ValuesApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> context;

  const ValuesApi({
    required this.networkingClient,
    required this.baseUrl,
    this.context = const {},
  });

  Future<ValuesApiGetSomeValuesResponse> getSomeValues(
    ValuesApiGetSomeValuesRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return ValuesApiGetSomeValuesResponse.fromResponse(
      response,
      context: newContext,
    );
  }
}

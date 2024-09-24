import 'package:petstore_api/_internal.dart';

part 'fake_classname_tags123_api.requests.dart';
part 'fake_classname_tags123_api.responses.dart';

class FakeClassnameTags123Api {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> userContext;

  const FakeClassnameTags123Api({
    required this.networkingClient,
    required this.baseUrl,
    this.userContext = const {},
  });

  Future<FakeClassnameTags123ApiTestClassnameResponse> testClassname(
    FakeClassnameTags123ApiTestClassnameRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeClassnameTags123ApiTestClassnameResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
}

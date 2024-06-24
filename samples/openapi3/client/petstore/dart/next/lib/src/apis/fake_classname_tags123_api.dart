import 'package:openapi/_internal.dart';

part 'fake_classname_tags123_api.models.dart';

class FakeClassnameTags123Api {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> context;

  const FakeClassnameTags123Api({
    required this.networkingClient,
    required this.baseUrl,
    this.context = const {},
  });

  Future<FakeClassnameTags123ApiTestClassnameResponse> testClassname(
    FakeClassnameTags123ApiTestClassnameRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeClassnameTags123ApiTestClassnameResponse.fromResponse(
      response,
      context: newContext,
    );
  }
}

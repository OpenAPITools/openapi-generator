import 'package:openapi/_internal.dart';

part 'default_api.requests.dart';
part 'default_api.responses.dart';

class DefaultApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> context;

  const DefaultApi({
    required this.networkingClient,
    required this.baseUrl,
    this.context = const {},
  });

  Future<DefaultApiFakeAnyOfWIthSameErasureGetResponse> fakeAnyOfWIthSameErasureGet(
    DefaultApiFakeAnyOfWIthSameErasureGetRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return DefaultApiFakeAnyOfWIthSameErasureGetResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<DefaultApiFakeOneOfWIthSameErasureGetResponse> fakeOneOfWIthSameErasureGet(
    DefaultApiFakeOneOfWIthSameErasureGetRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return DefaultApiFakeOneOfWIthSameErasureGetResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<DefaultApiFooGetResponse> fooGet(
    DefaultApiFooGetRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return DefaultApiFooGetResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<DefaultApiPetsMulticontentTestPostResponse> petsMulticontentTestPost(
    DefaultApiPetsMulticontentTestPostRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return DefaultApiPetsMulticontentTestPostResponse.fromResponse(
      response,
      context: newContext,
    );
  }
}

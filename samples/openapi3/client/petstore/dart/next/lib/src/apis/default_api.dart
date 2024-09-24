import 'package:petstore_api/_internal.dart';

part 'default_api.requests.dart';
part 'default_api.responses.dart';

class DefaultApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> userContext;

  const DefaultApi({
    required this.networkingClient,
    required this.baseUrl,
    this.userContext = const {},
  });

  Future<DefaultApiFakeAnyOfWIthSameErasureGetResponse> fakeAnyOfWIthSameErasureGet(
    DefaultApiFakeAnyOfWIthSameErasureGetRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return DefaultApiFakeAnyOfWIthSameErasureGetResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<DefaultApiFakeOneOfWIthSameErasureGetResponse> fakeOneOfWIthSameErasureGet(
    DefaultApiFakeOneOfWIthSameErasureGetRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return DefaultApiFakeOneOfWIthSameErasureGetResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<DefaultApiFooGetResponse> fooGet(
    DefaultApiFooGetRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return DefaultApiFooGetResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<DefaultApiPetsMulticontentTestPostResponse> petsMulticontentTestPost(
    DefaultApiPetsMulticontentTestPostRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return DefaultApiPetsMulticontentTestPostResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
}

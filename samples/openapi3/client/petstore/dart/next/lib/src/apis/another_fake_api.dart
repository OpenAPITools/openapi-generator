import 'package:petstore_api/_internal.dart';

part 'another_fake_api.requests.dart';
part 'another_fake_api.responses.dart';

class AnotherFakeApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> userContext;

  const AnotherFakeApi({
    required this.networkingClient,
    required this.baseUrl,
    this.userContext = const {},
  });

  Future<AnotherFakeApi$123testSpecialTagsResponse> $123testSpecialTags(
    AnotherFakeApi$123testSpecialTagsRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return AnotherFakeApi$123testSpecialTagsResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<AnotherFakeApiGetParameterArrayNumberResponse> getParameterArrayNumber(
    AnotherFakeApiGetParameterArrayNumberRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return AnotherFakeApiGetParameterArrayNumberResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<AnotherFakeApiGetParameterStringNumberResponse> getParameterStringNumber(
    AnotherFakeApiGetParameterStringNumberRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return AnotherFakeApiGetParameterStringNumberResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<AnotherFakeApiNullRequestBodyResponse> nullRequestBody(
    AnotherFakeApiNullRequestBodyRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return AnotherFakeApiNullRequestBodyResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
}

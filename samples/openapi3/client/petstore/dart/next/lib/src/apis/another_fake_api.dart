import 'package:openapi/_internal.dart';

part 'another_fake_api.models.dart';

class AnotherFakeApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> context;

  const AnotherFakeApi({
    required this.networkingClient,
    required this.baseUrl,
    this.context = const {},
  });

  Future<AnotherFakeApi$123testSpecialTagsResponse> $123testSpecialTags(
    AnotherFakeApi$123testSpecialTagsRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return AnotherFakeApi$123testSpecialTagsResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<AnotherFakeApiGetParameterArrayNumberResponse> getParameterArrayNumber(
    AnotherFakeApiGetParameterArrayNumberRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return AnotherFakeApiGetParameterArrayNumberResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<AnotherFakeApiGetParameterStringNumberResponse> getParameterStringNumber(
    AnotherFakeApiGetParameterStringNumberRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return AnotherFakeApiGetParameterStringNumberResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<AnotherFakeApiNullRequestBodyResponse> nullRequestBody(
    AnotherFakeApiNullRequestBodyRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return AnotherFakeApiNullRequestBodyResponse.fromResponse(
      response,
      context: newContext,
    );
  }
}

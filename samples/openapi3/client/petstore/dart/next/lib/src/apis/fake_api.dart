import 'dart:convert';
import 'package:meta/meta.dart';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';
import 'dart:typed_data';

part 'fake_api.models.dart';

class FakeApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> context;

  const FakeApi({
    required this.networkingClient,
    required this.baseUrl,
    this.context = const {},
  });

  Future<FakeApiFakeGetFreeFormObjectGetResponse> fakeGetFreeFormObjectGet(
    FakeApiFakeGetFreeFormObjectGetRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiFakeGetFreeFormObjectGetResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiFakeOuterBooleanSerializeResponse> fakeOuterBooleanSerialize(
    FakeApiFakeOuterBooleanSerializeRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiFakeOuterBooleanSerializeResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiFakeOuterCompositeSerializeResponse> fakeOuterCompositeSerialize(
    FakeApiFakeOuterCompositeSerializeRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiFakeOuterCompositeSerializeResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiFakeOuterNumberSerializeResponse> fakeOuterNumberSerialize(
    FakeApiFakeOuterNumberSerializeRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiFakeOuterNumberSerializeResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiFakeOuterStringSerializeResponse> fakeOuterStringSerialize(
    FakeApiFakeOuterStringSerializeRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiFakeOuterStringSerializeResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiFakeUploadRefRequestBodiesResponse> fakeUploadRefRequestBodies(
    FakeApiFakeUploadRefRequestBodiesRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiFakeUploadRefRequestBodiesResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiGetFakeArrayofenumsResponse> getFakeArrayofenums(
    FakeApiGetFakeArrayofenumsRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiGetFakeArrayofenumsResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiGetFakeHealthResponse> getFakeHealth(
    FakeApiGetFakeHealthRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiGetFakeHealthResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiGetParameterNameMappingResponse> getParameterNameMapping(
    FakeApiGetParameterNameMappingRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiGetParameterNameMappingResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiTestAdditionalPropertiesReferenceResponse> testAdditionalPropertiesReference(
    FakeApiTestAdditionalPropertiesReferenceRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiTestAdditionalPropertiesReferenceResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiTestBodyWithFileSchemaResponse> testBodyWithFileSchema(
    FakeApiTestBodyWithFileSchemaRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiTestBodyWithFileSchemaResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiTestBodyWithQueryParamsResponse> testBodyWithQueryParams(
    FakeApiTestBodyWithQueryParamsRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiTestBodyWithQueryParamsResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiTestClientModelResponse> testClientModel(
    FakeApiTestClientModelRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiTestClientModelResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiTestEndpointParametersResponse> testEndpointParameters(
    FakeApiTestEndpointParametersRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiTestEndpointParametersResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiTestEnumParametersResponse> testEnumParameters(
    FakeApiTestEnumParametersRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiTestEnumParametersResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiTestGroupParametersResponse> testGroupParameters(
    FakeApiTestGroupParametersRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiTestGroupParametersResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiTestInlineAdditionalPropertiesResponse> testInlineAdditionalProperties(
    FakeApiTestInlineAdditionalPropertiesRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiTestInlineAdditionalPropertiesResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiTestInlineFreeformAdditionalPropertiesResponse> testInlineFreeformAdditionalProperties(
    FakeApiTestInlineFreeformAdditionalPropertiesRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiTestInlineFreeformAdditionalPropertiesResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiTestJsonFormDataResponse> testJsonFormData(
    FakeApiTestJsonFormDataRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiTestJsonFormDataResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiTestQueryParameterCollectionFormatResponse> testQueryParameterCollectionFormat(
    FakeApiTestQueryParameterCollectionFormatRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiTestQueryParameterCollectionFormatResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<FakeApiTestStringMapReferenceResponse> testStringMapReference(
    FakeApiTestStringMapReferenceRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return FakeApiTestStringMapReferenceResponse.fromResponse(
      response,
      context: newContext,
    );
  }
}

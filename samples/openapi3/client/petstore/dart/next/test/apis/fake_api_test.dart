import 'package:test/test.dart';
import 'package:petstore_api/_internal.dart';

Stream<List<int>> _streamOfText(String value, {Encoding? encoding}) async* {
  yield (encoding ?? utf8).encode(value);
}

final baseUrl = Uri.https("example.com", "/api");
HttpResponseBase createFakeResponse({
  required int statusCode,
  required Stream<List<int>> bodyBytesStream,
  Map<String, String> headers = const {},
  String? reasonPhrase,
}) {
  return HttpResponseBase.stream(
    originalRequest: HttpRequestBase.empty(url: baseUrl, method: 'GET'),
    bodyBytesStream: bodyBytesStream,
    statusCode: statusCode,
    reasonPhrase: reasonPhrase,
    headers: headers,
  );
}

HttpResponseBase createFakeTextResponse({
  required int statusCode,
  required String value,
  Map<String, String> headers = const {},
  String? contentType,
  Encoding? encoding,
  String? reasonPhrase,
}) {
  var parsedContentType =
      contentType == null ? null : MediaType.parse(contentType);
  if (encoding != null) {
    parsedContentType = parsedContentType?.change(parameters: {
      ...parsedContentType.parameters,
      'charset': encoding.name,
    });
  }
  return createFakeResponse(
    statusCode: statusCode,
    headers: {
      'Content-Type': 'text/plain; charset=utf-8',
      if (parsedContentType != null)
        'Content-Type': parsedContentType.toString(),
      ...headers,
    },
    reasonPhrase: reasonPhrase,
    bodyBytesStream: _streamOfText(value, encoding: encoding),
  );
}

/// tests for FakeApi
void main() {
  group(FakeApi, () {
    final baseUrl = Uri.https("example.com", "/api");
    // Get a free form object or Json string
    //
    group(r'fakeGetFreeFormObjectGet', () {
      group(FakeApiFakeGetFreeFormObjectGetRequest, () {
    late FakeApiFakeGetFreeFormObjectGetRequest request;
    test(r'No Body', () async {
        request = FakeApiFakeGetFreeFormObjectGetRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(FakeApiFakeGetFreeFormObjectGetResponse, () {
  late FakeApiFakeGetFreeFormObjectGetResponse response;
  test('Unkown status code', () async {
    response = await FakeApiFakeGetFreeFormObjectGetResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiFakeGetFreeFormObjectGetResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(FakeApiFakeGetFreeFormObjectGetResponse200ApplicationJson, () async {

    });
    
  });

});
    });
    // Test serialization of outer boolean types
    //
    group(r'fakeOuterBooleanSerialize', () {
      group(FakeApiFakeOuterBooleanSerializeRequest, () {
    late FakeApiFakeOuterBooleanSerializeRequest request;
    test(FakeApiFakeOuterBooleanSerializeRequestUnsafe, () async {
        request = FakeApiFakeOuterBooleanSerializeRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(FakeApiFakeOuterBooleanSerializeRequestApplicationJson, () async {
        request = FakeApiFakeOuterBooleanSerializeRequest.applicationJson(
            data: 


            
            


    
    examplebool()


,
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiFakeOuterBooleanSerializeResponse, () {
  late FakeApiFakeOuterBooleanSerializeResponse response;
  test('Unkown status code', () async {
    response = await FakeApiFakeOuterBooleanSerializeResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiFakeOuterBooleanSerializeResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(FakeApiFakeOuterBooleanSerializeResponse200AnyAny, () async {

    });
    
  });

});
    });
    // Test serialization of object with outer number type
    //
    group(r'fakeOuterCompositeSerialize', () {
      group(FakeApiFakeOuterCompositeSerializeRequest, () {
    late FakeApiFakeOuterCompositeSerializeRequest request;
    test(FakeApiFakeOuterCompositeSerializeRequestUnsafe, () async {
        request = FakeApiFakeOuterCompositeSerializeRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(FakeApiFakeOuterCompositeSerializeRequestApplicationJson, () async {
        request = FakeApiFakeOuterCompositeSerializeRequest.applicationJson(
            data: 


            
            


    OuterComposite.$reflection.example()
    


,
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiFakeOuterCompositeSerializeResponse, () {
  late FakeApiFakeOuterCompositeSerializeResponse response;
  test('Unkown status code', () async {
    response = await FakeApiFakeOuterCompositeSerializeResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiFakeOuterCompositeSerializeResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(FakeApiFakeOuterCompositeSerializeResponse200AnyAny, () async {

    });
    
  });

});
    });
    // Test serialization of outer number types
    //
    group(r'fakeOuterNumberSerialize', () {
      group(FakeApiFakeOuterNumberSerializeRequest, () {
    late FakeApiFakeOuterNumberSerializeRequest request;
    test(FakeApiFakeOuterNumberSerializeRequestUnsafe, () async {
        request = FakeApiFakeOuterNumberSerializeRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(FakeApiFakeOuterNumberSerializeRequestApplicationJson, () async {
        request = FakeApiFakeOuterNumberSerializeRequest.applicationJson(
            data: 


            
            


    
    examplenum()


,
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiFakeOuterNumberSerializeResponse, () {
  late FakeApiFakeOuterNumberSerializeResponse response;
  test('Unkown status code', () async {
    response = await FakeApiFakeOuterNumberSerializeResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiFakeOuterNumberSerializeResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(FakeApiFakeOuterNumberSerializeResponse200AnyAny, () async {

    });
    
  });

});
    });
    // Test serialization of outer string types
    //
    group(r'fakeOuterStringSerialize', () {
      group(FakeApiFakeOuterStringSerializeRequest, () {
    late FakeApiFakeOuterStringSerializeRequest request;
    test(FakeApiFakeOuterStringSerializeRequestUnsafe, () async {
        request = FakeApiFakeOuterStringSerializeRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(FakeApiFakeOuterStringSerializeRequestApplicationJson, () async {
        request = FakeApiFakeOuterStringSerializeRequest.applicationJson(
            data: 


            
            


    
    exampleString()


,
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiFakeOuterStringSerializeResponse, () {
  late FakeApiFakeOuterStringSerializeResponse response;
  test('Unkown status code', () async {
    response = await FakeApiFakeOuterStringSerializeResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiFakeOuterStringSerializeResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(FakeApiFakeOuterStringSerializeResponse200AnyAny, () async {

    });
    
  });

});
    });
    // fake uploads an image with ref request bodies
    //
    // 
    //
    group(r'fakeUploadRefRequestBodies', () {
      group(FakeApiFakeUploadRefRequestBodiesRequest, () {
    late FakeApiFakeUploadRefRequestBodiesRequest request;
    test(FakeApiFakeUploadRefRequestBodiesRequestUnsafe, () async {
        request = FakeApiFakeUploadRefRequestBodiesRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
petId:  


            
            


    
    exampleint()


,

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiFakeUploadRefRequestBodiesResponse, () {
  late FakeApiFakeUploadRefRequestBodiesResponse response;
  test('Unkown status code', () async {
    response = await FakeApiFakeUploadRefRequestBodiesResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiFakeUploadRefRequestBodiesResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(FakeApiFakeUploadRefRequestBodiesResponse200ApplicationJson, () async {

    });
    
  });

});
    });
    // Array of Enums
    //
    group(r'getFakeArrayofenums', () {
      group(FakeApiGetFakeArrayofenumsRequest, () {
    late FakeApiGetFakeArrayofenumsRequest request;
    test(r'No Body', () async {
        request = FakeApiGetFakeArrayofenumsRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(FakeApiGetFakeArrayofenumsResponse, () {
  late FakeApiGetFakeArrayofenumsResponse response;
  test('Unkown status code', () async {
    response = await FakeApiGetFakeArrayofenumsResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiGetFakeArrayofenumsResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(FakeApiGetFakeArrayofenumsResponse200ApplicationJson, () async {

    });
    
  });

});
    });
    // Health check endpoint
    //
    group(r'getFakeHealth', () {
      group(FakeApiGetFakeHealthRequest, () {
    late FakeApiGetFakeHealthRequest request;
    test(r'No Body', () async {
        request = FakeApiGetFakeHealthRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(FakeApiGetFakeHealthResponse, () {
  late FakeApiGetFakeHealthResponse response;
  test('Unkown status code', () async {
    response = await FakeApiGetFakeHealthResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiGetFakeHealthResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(FakeApiGetFakeHealthResponse200ApplicationJson, () async {

    });
    
  });

});
    });
    // parameter name mapping test
    //
    group(r'getParameterNameMapping', () {
      group(FakeApiGetParameterNameMappingRequest, () {
    late FakeApiGetParameterNameMappingRequest request;
    test(r'No Body', () async {
        request = FakeApiGetParameterNameMappingRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
$type:  


            
            


    
    exampleint()


,

type:  


            
            


    
    exampleString()


,

type$:  


            
            


    
    exampleString()


,

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(FakeApiGetParameterNameMappingResponse, () {
  late FakeApiGetParameterNameMappingResponse response;
  test('Unkown status code', () async {
    response = await FakeApiGetParameterNameMappingResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiGetParameterNameMappingResponse200, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // test referenced additionalProperties
    //
    // 
    //
    group(r'testAdditionalPropertiesReference', () {
      group(FakeApiTestAdditionalPropertiesReferenceRequest, () {
    late FakeApiTestAdditionalPropertiesReferenceRequest request;
    test(FakeApiTestAdditionalPropertiesReferenceRequestUnsafe, () async {
        request = FakeApiTestAdditionalPropertiesReferenceRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(FakeApiTestAdditionalPropertiesReferenceRequestApplicationJson, () async {
        request = FakeApiTestAdditionalPropertiesReferenceRequest.applicationJson(
            data: 


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return exampleNullable(() =>

exampleObject()



 ) ; })



,
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiTestAdditionalPropertiesReferenceResponse, () {
  late FakeApiTestAdditionalPropertiesReferenceResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestAdditionalPropertiesReferenceResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestAdditionalPropertiesReferenceResponse200, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // For this test, the body for this request much reference a schema named `File`.
    //
    group(r'testBodyWithFileSchema', () {
      group(FakeApiTestBodyWithFileSchemaRequest, () {
    late FakeApiTestBodyWithFileSchemaRequest request;
    test(FakeApiTestBodyWithFileSchemaRequestUnsafe, () async {
        request = FakeApiTestBodyWithFileSchemaRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(FakeApiTestBodyWithFileSchemaRequestApplicationJson, () async {
        request = FakeApiTestBodyWithFileSchemaRequest.applicationJson(
            data: 


            
            


    FileSchemaTestClass.$reflection.example()
    


,
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiTestBodyWithFileSchemaResponse, () {
  late FakeApiTestBodyWithFileSchemaResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestBodyWithFileSchemaResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestBodyWithFileSchemaResponse200, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    group(r'testBodyWithQueryParams', () {
      group(FakeApiTestBodyWithQueryParamsRequest, () {
    late FakeApiTestBodyWithQueryParamsRequest request;
    test(FakeApiTestBodyWithQueryParamsRequestUnsafe, () async {
        request = FakeApiTestBodyWithQueryParamsRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
query:  


            
            


    
    exampleString()


,

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(FakeApiTestBodyWithQueryParamsRequestApplicationJson, () async {
        request = FakeApiTestBodyWithQueryParamsRequest.applicationJson(
            data: 


            
            


    User.$reflection.example()
    


,
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
query:  


            
            


    
    exampleString()


,

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiTestBodyWithQueryParamsResponse, () {
  late FakeApiTestBodyWithQueryParamsResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestBodyWithQueryParamsResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestBodyWithQueryParamsResponse200, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // To test \"client\" model
    //
    // To test \"client\" model
    //
    group(r'testClientModel', () {
      group(FakeApiTestClientModelRequest, () {
    late FakeApiTestClientModelRequest request;
    test(FakeApiTestClientModelRequestUnsafe, () async {
        request = FakeApiTestClientModelRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(FakeApiTestClientModelRequestApplicationJson, () async {
        request = FakeApiTestClientModelRequest.applicationJson(
            data: 


            
            


    Client.$reflection.example()
    


,
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiTestClientModelResponse, () {
  late FakeApiTestClientModelResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestClientModelResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestClientModelResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(FakeApiTestClientModelResponse200ApplicationJson, () async {

    });
    
  });

});
    });
    // Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    //
    // Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    //
    group(r'testEndpointParameters', () {
      group(FakeApiTestEndpointParametersRequest, () {
    late FakeApiTestEndpointParametersRequest request;
    test(FakeApiTestEndpointParametersRequestUnsafe, () async {
        request = FakeApiTestEndpointParametersRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiTestEndpointParametersResponse, () {
  late FakeApiTestEndpointParametersResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestEndpointParametersResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestEndpointParametersResponse400, () {
    test('Unkown mime', () async {

    });
    
  });
  group(FakeApiTestEndpointParametersResponse404, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // To test enum parameters
    //
    // To test enum parameters
    //
    group(r'testEnumParameters', () {
      group(FakeApiTestEnumParametersRequest, () {
    late FakeApiTestEnumParametersRequest request;
    test(r'No Body', () async {
        request = FakeApiTestEnumParametersRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
enumHeaderStringArray: UndefinedWrapper( 


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            exampleEnum(EnumHeaderStringArrayEnum.values)



; })



),

enumHeaderString: UndefinedWrapper( 


            exampleEnum(EnumHeaderStringEnum.values)



),

enumQueryStringArray: UndefinedWrapper( 


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            exampleEnum(EnumQueryStringArrayEnum.values)



; })



),

enumQueryString: UndefinedWrapper( 


            exampleEnum(EnumQueryStringEnum.values)



),

enumQueryInteger: UndefinedWrapper( 


            exampleEnum(EnumQueryIntegerEnum.values)



),

enumQueryDouble: UndefinedWrapper( 


            exampleEnum(EnumQueryDoubleEnum.values)



),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(FakeApiTestEnumParametersResponse, () {
  late FakeApiTestEnumParametersResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestEnumParametersResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestEnumParametersResponse400, () {
    test('Unkown mime', () async {

    });
    
  });
  group(FakeApiTestEnumParametersResponse404, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // Fake endpoint to test group parameters (optional)
    //
    // Fake endpoint to test group parameters (optional)
    //
    group(r'testGroupParameters', () {
      group(FakeApiTestGroupParametersRequest, () {
    late FakeApiTestGroupParametersRequest request;
    test(r'No Body', () async {
        request = FakeApiTestGroupParametersRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
requiredStringGroup:  


            
            


    
    exampleint()


,

requiredBooleanGroup:  


            
            


    
    examplebool()


,

requiredInt64Group:  


            
            


    
    exampleint()


,

stringGroup: UndefinedWrapper( 


            
            


    
    exampleint()


),

booleanGroup: UndefinedWrapper( 


            
            


    
    examplebool()


),

int64Group: UndefinedWrapper( 


            
            


    
    exampleint()


),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(FakeApiTestGroupParametersResponse, () {
  late FakeApiTestGroupParametersResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestGroupParametersResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestGroupParametersResponse400, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // test inline additionalProperties
    //
    // 
    //
    group(r'testInlineAdditionalProperties', () {
      group(FakeApiTestInlineAdditionalPropertiesRequest, () {
    late FakeApiTestInlineAdditionalPropertiesRequest request;
    test(FakeApiTestInlineAdditionalPropertiesRequestUnsafe, () async {
        request = FakeApiTestInlineAdditionalPropertiesRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(FakeApiTestInlineAdditionalPropertiesRequestApplicationJson, () async {
        request = FakeApiTestInlineAdditionalPropertiesRequest.applicationJson(
            data: 


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



,
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiTestInlineAdditionalPropertiesResponse, () {
  late FakeApiTestInlineAdditionalPropertiesResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestInlineAdditionalPropertiesResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestInlineAdditionalPropertiesResponse200, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // test inline free-form additionalProperties
    //
    // 
    //
    group(r'testInlineFreeformAdditionalProperties', () {
      group(FakeApiTestInlineFreeformAdditionalPropertiesRequest, () {
    late FakeApiTestInlineFreeformAdditionalPropertiesRequest request;
    test(FakeApiTestInlineFreeformAdditionalPropertiesRequestUnsafe, () async {
        request = FakeApiTestInlineFreeformAdditionalPropertiesRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(FakeApiTestInlineFreeformAdditionalPropertiesRequestApplicationJson, () async {
        request = FakeApiTestInlineFreeformAdditionalPropertiesRequest.applicationJson(
            data: 


            
            


    TestInlineFreeformAdditionalPropertiesRequest.$reflection.example()
    


,
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiTestInlineFreeformAdditionalPropertiesResponse, () {
  late FakeApiTestInlineFreeformAdditionalPropertiesResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestInlineFreeformAdditionalPropertiesResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestInlineFreeformAdditionalPropertiesResponse200, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // test json serialization of form data
    //
    // 
    //
    group(r'testJsonFormData', () {
      group(FakeApiTestJsonFormDataRequest, () {
    late FakeApiTestJsonFormDataRequest request;
    test(r'No Body', () async {
        request = FakeApiTestJsonFormDataRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(FakeApiTestJsonFormDataResponse, () {
  late FakeApiTestJsonFormDataResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestJsonFormDataResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestJsonFormDataResponse200, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // To test the collection format in query parameters
    //
    group(r'testQueryParameterCollectionFormat', () {
      group(FakeApiTestQueryParameterCollectionFormatRequest, () {
    late FakeApiTestQueryParameterCollectionFormatRequest request;
    test(FakeApiTestQueryParameterCollectionFormatRequestUnsafe, () async {
        request = FakeApiTestQueryParameterCollectionFormatRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
pipe:  


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



,

ioutil:  


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



,

http:  


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



,

url:  


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



,

context:  


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



,

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiTestQueryParameterCollectionFormatResponse, () {
  late FakeApiTestQueryParameterCollectionFormatResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestQueryParameterCollectionFormatResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestQueryParameterCollectionFormatResponse200, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // test referenced string map
    //
    // 
    //
    group(r'testStringMapReference', () {
      group(FakeApiTestStringMapReferenceRequest, () {
    late FakeApiTestStringMapReferenceRequest request;
    test(FakeApiTestStringMapReferenceRequestUnsafe, () async {
        request = FakeApiTestStringMapReferenceRequest.unsafe(
            bodyBytesStream: Stream.value(Uint8List.fromList([1, 2, 3])),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(FakeApiTestStringMapReferenceRequestApplicationJson, () async {
        request = FakeApiTestStringMapReferenceRequest.applicationJson(
            data: 


    exampleMap(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



,
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(FakeApiTestStringMapReferenceResponse, () {
  late FakeApiTestStringMapReferenceResponse response;
  test('Unkown status code', () async {
    response = await FakeApiTestStringMapReferenceResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(FakeApiTestStringMapReferenceResponse200, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
  });
}

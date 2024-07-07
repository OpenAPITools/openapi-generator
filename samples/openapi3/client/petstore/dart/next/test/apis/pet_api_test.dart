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

/// tests for PetApi
void main() {
  group(PetApi, () {
    final baseUrl = Uri.https("example.com", "/api");
    // Add a new pet to the store
    //
    // 
    //
    group(r'addPet', () {
      group(PetApiAddPetRequest, () {
    late PetApiAddPetRequest request;
    test(PetApiAddPetRequestUnsafe, () async {
        request = PetApiAddPetRequest.unsafe(
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
    
    test(PetApiAddPetRequestApplicationJson, () async {
        request = PetApiAddPetRequest.applicationJson(
            data: 


            
            


    Pet.$reflection.example()
    


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
    
    test(PetApiAddPetRequestApplicationXml, () async {
        request = PetApiAddPetRequest.applicationXml(
            data: 


            
            


    Pet.$reflection.example()
    


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

      group(PetApiAddPetResponse, () {
  late PetApiAddPetResponse response;
  test('Unkown status code', () async {
    response = await PetApiAddPetResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(PetApiAddPetResponse405, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // Deletes a pet
    //
    // 
    //
    group(r'deletePet', () {
      group(PetApiDeletePetRequest, () {
    late PetApiDeletePetRequest request;
    test(r'No Body', () async {
        request = PetApiDeletePetRequest(
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

apiKey: UndefinedWrapper( 


            
            


    
    exampleString()


),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(PetApiDeletePetResponse, () {
  late PetApiDeletePetResponse response;
  test('Unkown status code', () async {
    response = await PetApiDeletePetResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(PetApiDeletePetResponse400, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // Finds Pets by status
    //
    // Multiple status values can be provided with comma separated strings
    //
    group(r'findPetsByStatus', () {
      group(PetApiFindPetsByStatusRequest, () {
    late PetApiFindPetsByStatusRequest request;
    test(r'No Body', () async {
        request = PetApiFindPetsByStatusRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
status:  


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            exampleEnum(StatusEnum.values)



; })



,

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(PetApiFindPetsByStatusResponse, () {
  late PetApiFindPetsByStatusResponse response;
  test('Unkown status code', () async {
    response = await PetApiFindPetsByStatusResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(PetApiFindPetsByStatusResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(PetApiFindPetsByStatusResponse200ApplicationXml, () async {

    });
    
    test(PetApiFindPetsByStatusResponse200ApplicationJson, () async {

    });
    
  });
  group(PetApiFindPetsByStatusResponse400, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // Finds Pets by tags
    //
    // Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    //
    group(r'findPetsByTags', () {
      group(PetApiFindPetsByTagsRequest, () {
    late PetApiFindPetsByTagsRequest request;
    test(r'No Body', () async {
        request = PetApiFindPetsByTagsRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
tags:  


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



,

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(PetApiFindPetsByTagsResponse, () {
  late PetApiFindPetsByTagsResponse response;
  test('Unkown status code', () async {
    response = await PetApiFindPetsByTagsResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(PetApiFindPetsByTagsResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(PetApiFindPetsByTagsResponse200ApplicationXml, () async {

    });
    
    test(PetApiFindPetsByTagsResponse200ApplicationJson, () async {

    });
    
  });
  group(PetApiFindPetsByTagsResponse400, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // Find pet by ID
    //
    // Returns a single pet
    //
    group(r'getPetById', () {
      group(PetApiGetPetByIdRequest, () {
    late PetApiGetPetByIdRequest request;
    test(r'No Body', () async {
        request = PetApiGetPetByIdRequest(
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

      group(PetApiGetPetByIdResponse, () {
  late PetApiGetPetByIdResponse response;
  test('Unkown status code', () async {
    response = await PetApiGetPetByIdResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(PetApiGetPetByIdResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(PetApiGetPetByIdResponse200ApplicationXml, () async {

    });
    
    test(PetApiGetPetByIdResponse200ApplicationJson, () async {

    });
    
  });
  group(PetApiGetPetByIdResponse400, () {
    test('Unkown mime', () async {

    });
    
  });
  group(PetApiGetPetByIdResponse404, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // Update an existing pet
    //
    // 
    //
    group(r'updatePet', () {
      group(PetApiUpdatePetRequest, () {
    late PetApiUpdatePetRequest request;
    test(PetApiUpdatePetRequestUnsafe, () async {
        request = PetApiUpdatePetRequest.unsafe(
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
    
    test(PetApiUpdatePetRequestApplicationJson, () async {
        request = PetApiUpdatePetRequest.applicationJson(
            data: 


            
            


    Pet.$reflection.example()
    


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
    
    test(PetApiUpdatePetRequestApplicationXml, () async {
        request = PetApiUpdatePetRequest.applicationXml(
            data: 


            
            


    Pet.$reflection.example()
    


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

      group(PetApiUpdatePetResponse, () {
  late PetApiUpdatePetResponse response;
  test('Unkown status code', () async {
    response = await PetApiUpdatePetResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(PetApiUpdatePetResponse400, () {
    test('Unkown mime', () async {

    });
    
  });
  group(PetApiUpdatePetResponse404, () {
    test('Unkown mime', () async {

    });
    
  });
  group(PetApiUpdatePetResponse405, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // Updates a pet in the store with form data
    //
    // 
    //
    group(r'updatePetWithForm', () {
      group(PetApiUpdatePetWithFormRequest, () {
    late PetApiUpdatePetWithFormRequest request;
    test(PetApiUpdatePetWithFormRequestUnsafe, () async {
        request = PetApiUpdatePetWithFormRequest.unsafe(
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

      group(PetApiUpdatePetWithFormResponse, () {
  late PetApiUpdatePetWithFormResponse response;
  test('Unkown status code', () async {
    response = await PetApiUpdatePetWithFormResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(PetApiUpdatePetWithFormResponse405, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
    // uploads an image
    //
    // 
    //
    group(r'uploadFile', () {
      group(PetApiUploadFileRequest, () {
    late PetApiUploadFileRequest request;
    test(PetApiUploadFileRequestUnsafe, () async {
        request = PetApiUploadFileRequest.unsafe(
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

      group(PetApiUploadFileResponse, () {
  late PetApiUploadFileResponse response;
  test('Unkown status code', () async {
    response = await PetApiUploadFileResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(PetApiUploadFileResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(PetApiUploadFileResponse200ApplicationJson, () async {

    });
    
  });

});
    });
    // uploads an image (required)
    //
    // 
    //
    group(r'uploadFileWithRequiredFile', () {
      group(PetApiUploadFileWithRequiredFileRequest, () {
    late PetApiUploadFileWithRequiredFileRequest request;
    test(PetApiUploadFileWithRequiredFileRequestUnsafe, () async {
        request = PetApiUploadFileWithRequiredFileRequest.unsafe(
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

      group(PetApiUploadFileWithRequiredFileResponse, () {
  late PetApiUploadFileWithRequiredFileResponse response;
  test('Unkown status code', () async {
    response = await PetApiUploadFileWithRequiredFileResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(PetApiUploadFileWithRequiredFileResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(PetApiUploadFileWithRequiredFileResponse200ApplicationJson, () async {

    });
    
  });

});
    });
  });
}

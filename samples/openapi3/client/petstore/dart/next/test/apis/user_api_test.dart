import 'package:test/test.dart';
import 'package:petstore_api/_internal.dart';
import '../utils.dart';

/// tests for UserApi
void main() {
  final wireOptions = WireSerializationOptions();
  final exampleContext = ExampleContext();
  group(UserApi, () {
    final baseUrl = Uri.https("example.com", "/api");
    // Create user
    //
    // This can only be done by the logged in user.
    //
    group(r'createUser', () {
      group(UserApiCreateUserRequest, () {
    late UserApiCreateUserRequest request;
    test(UserApiCreateUserRequestUnsafe, () async {
        request = UserApiCreateUserRequest.unsafe(
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
    
    test(UserApiCreateUserRequestApplicationJson, () async {
        request = UserApiCreateUserRequest.applicationJson(
            data: UserApiCreateUserRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(UserApiCreateUserResponse, () {
  late UserApiCreateUserResponse response;
  test('Unkown status code', () async {
    response = await UserApiCreateUserResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<UserApiCreateUserResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(UserApiCreateUserResponseDefault, () {
    test('Unknown mime', () async {
      final codeExample = exampleContext.exampleCode(1, 599)
;
      response = await UserApiCreateUserResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiCreateUserResponseDefault>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Creates list of users with given input array
    //
    // 
    //
    group(r'createUsersWithArrayInput', () {
      group(UserApiCreateUsersWithArrayInputRequest, () {
    late UserApiCreateUsersWithArrayInputRequest request;
    test(UserApiCreateUsersWithArrayInputRequestUnsafe, () async {
        request = UserApiCreateUsersWithArrayInputRequest.unsafe(
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
    
    test(UserApiCreateUsersWithArrayInputRequestApplicationJson, () async {
        request = UserApiCreateUsersWithArrayInputRequest.applicationJson(
            data: UserApiCreateUsersWithArrayInputRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(UserApiCreateUsersWithArrayInputResponse, () {
  late UserApiCreateUsersWithArrayInputResponse response;
  test('Unkown status code', () async {
    response = await UserApiCreateUsersWithArrayInputResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<UserApiCreateUsersWithArrayInputResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(UserApiCreateUsersWithArrayInputResponseDefault, () {
    test('Unknown mime', () async {
      final codeExample = exampleContext.exampleCode(1, 599)
;
      response = await UserApiCreateUsersWithArrayInputResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiCreateUsersWithArrayInputResponseDefault>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Creates list of users with given input array
    //
    // 
    //
    group(r'createUsersWithListInput', () {
      group(UserApiCreateUsersWithListInputRequest, () {
    late UserApiCreateUsersWithListInputRequest request;
    test(UserApiCreateUsersWithListInputRequestUnsafe, () async {
        request = UserApiCreateUsersWithListInputRequest.unsafe(
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
    
    test(UserApiCreateUsersWithListInputRequestApplicationJson, () async {
        request = UserApiCreateUsersWithListInputRequest.applicationJson(
            data: UserApiCreateUsersWithListInputRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
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

      group(UserApiCreateUsersWithListInputResponse, () {
  late UserApiCreateUsersWithListInputResponse response;
  test('Unkown status code', () async {
    response = await UserApiCreateUsersWithListInputResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<UserApiCreateUsersWithListInputResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(UserApiCreateUsersWithListInputResponseDefault, () {
    test('Unknown mime', () async {
      final codeExample = exampleContext.exampleCode(1, 599)
;
      response = await UserApiCreateUsersWithListInputResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiCreateUsersWithListInputResponseDefault>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Delete user
    //
    // This can only be done by the logged in user.
    //
    group(r'deleteUser', () {
      group(UserApiDeleteUserRequest, () {
    late UserApiDeleteUserRequest request;
    test(r'No Body', () async {
        request = UserApiDeleteUserRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
username: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
.exampleFunction(exampleContext),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(UserApiDeleteUserResponse, () {
  late UserApiDeleteUserResponse response;
  test('Unkown status code', () async {
    response = await UserApiDeleteUserResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<UserApiDeleteUserResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(UserApiDeleteUserResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await UserApiDeleteUserResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiDeleteUserResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });
  group(UserApiDeleteUserResponse404, () {
    test('Unknown mime', () async {
      final codeExample = 
    404
;
      response = await UserApiDeleteUserResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiDeleteUserResponse404>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Get user by user name
    //
    // 
    //
    group(r'getUserByName', () {
      group(UserApiGetUserByNameRequest, () {
    late UserApiGetUserByNameRequest request;
    test(r'No Body', () async {
        request = UserApiGetUserByNameRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
username: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
.exampleFunction(exampleContext),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(UserApiGetUserByNameResponse, () {
  late UserApiGetUserByNameResponse response;
  test('Unkown status code', () async {
    response = await UserApiGetUserByNameResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<UserApiGetUserByNameResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(UserApiGetUserByNameResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await UserApiGetUserByNameResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiGetUserByNameResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(UserApiGetUserByNameResponse200ApplicationXml, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/xml'));
      final bodyReflection = UserApiGetUserByNameResponse200ApplicationXml.bodyReflection;
      // v: an example response body.
      final v = bodyReflection.exampleFunction(exampleContext);
      SerializationContext context;
      if (wireOptions.isXml(mimeExample)) {
        context = const SerializationContext.xml();
      } else {
        context = const SerializationContext.json();
      }
      final serializedBody = bodyReflection.serializeFunction(v, context);
      Object? finalValue = null;
      finalValue = serializedBody;

      response = await UserApiGetUserByNameResponse.fromResponse(
        switch (finalValue) {
          String() => createFakeTextResponse(
            statusCode: codeExample,
            value: finalValue,
            contentType: mimeExample.toString(),
          ),
          _ => createFakeTextResponse(
            value: serializedBody.toString(),
            statusCode: codeExample,
            contentType: mimeExample.toString(),
          ),
        },
        userContext: {},
        wireSerializationOptions: wireOptions,
      );
      expect(response.statusCode, codeExample);
      expect(response.headers, containsPair('content-type', mimeExample.toString()));
      //bodyBytesStream SHOULD be null if the response was handled successfully.
      expect(response.bodyBytesStream, OASNetworkingUtils.isMediaTypeSerializable(mimeExample) ? isNull : isNotNull);
    });
    
    test(UserApiGetUserByNameResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = UserApiGetUserByNameResponse200ApplicationJson.bodyReflection;
      // v: an example response body.
      final v = bodyReflection.exampleFunction(exampleContext);
      SerializationContext context;
      if (wireOptions.isXml(mimeExample)) {
        context = const SerializationContext.xml();
      } else {
        context = const SerializationContext.json();
      }
      final serializedBody = bodyReflection.serializeFunction(v, context);
      Object? finalValue = null;
      finalValue = jsonEncode(serializedBody, toEncodable: wireOptions.toEncodable);

      response = await UserApiGetUserByNameResponse.fromResponse(
        switch (finalValue) {
          String() => createFakeTextResponse(
            statusCode: codeExample,
            value: finalValue,
            contentType: mimeExample.toString(),
          ),
          _ => createFakeTextResponse(
            value: serializedBody.toString(),
            statusCode: codeExample,
            contentType: mimeExample.toString(),
          ),
        },
        userContext: {},
        wireSerializationOptions: wireOptions,
      );
      expect(response.statusCode, codeExample);
      expect(response.headers, containsPair('content-type', mimeExample.toString()));
      //bodyBytesStream SHOULD be null if the response was handled successfully.
      expect(response.bodyBytesStream, OASNetworkingUtils.isMediaTypeSerializable(mimeExample) ? isNull : isNotNull);
    });
    
  });
  group(UserApiGetUserByNameResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await UserApiGetUserByNameResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiGetUserByNameResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });
  group(UserApiGetUserByNameResponse404, () {
    test('Unknown mime', () async {
      final codeExample = 
    404
;
      response = await UserApiGetUserByNameResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiGetUserByNameResponse404>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Logs user into the system
    //
    // 
    //
    group(r'loginUser', () {
      group(UserApiLoginUserRequest, () {
    late UserApiLoginUserRequest request;
    test(r'No Body', () async {
        request = UserApiLoginUserRequest(
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
username: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
.exampleFunction(exampleContext),

password: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
.exampleFunction(exampleContext),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
});

      group(UserApiLoginUserResponse, () {
  late UserApiLoginUserResponse response;
  test('Unkown status code', () async {
    response = await UserApiLoginUserResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<UserApiLoginUserResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(UserApiLoginUserResponse200, () {
    test('Unknown mime', () async {
      final codeExample = 
    200
;
      response = await UserApiLoginUserResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiLoginUserResponse200>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
    test(UserApiLoginUserResponse200ApplicationXml, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/xml'));
      final bodyReflection = UserApiLoginUserResponse200ApplicationXml.bodyReflection;
      // v: an example response body.
      final v = bodyReflection.exampleFunction(exampleContext);
      SerializationContext context;
      if (wireOptions.isXml(mimeExample)) {
        context = const SerializationContext.xml();
      } else {
        context = const SerializationContext.json();
      }
      final serializedBody = bodyReflection.serializeFunction(v, context);
      Object? finalValue = null;
      finalValue = serializedBody;

      response = await UserApiLoginUserResponse.fromResponse(
        switch (finalValue) {
          String() => createFakeTextResponse(
            statusCode: codeExample,
            value: finalValue,
            contentType: mimeExample.toString(),
          ),
          _ => createFakeTextResponse(
            value: serializedBody.toString(),
            statusCode: codeExample,
            contentType: mimeExample.toString(),
          ),
        },
        userContext: {},
        wireSerializationOptions: wireOptions,
      );
      expect(response.statusCode, codeExample);
      expect(response.headers, containsPair('content-type', mimeExample.toString()));
      //bodyBytesStream SHOULD be null if the response was handled successfully.
      expect(response.bodyBytesStream, OASNetworkingUtils.isMediaTypeSerializable(mimeExample) ? isNull : isNotNull);
    });
    
    test(UserApiLoginUserResponse200ApplicationJson, () async {
      final codeExample = 
    200
;
      final mimeExample = MediaType.parse(exampleContext.exampleMimeType(r'application/json'));
      final bodyReflection = UserApiLoginUserResponse200ApplicationJson.bodyReflection;
      // v: an example response body.
      final v = bodyReflection.exampleFunction(exampleContext);
      SerializationContext context;
      if (wireOptions.isXml(mimeExample)) {
        context = const SerializationContext.xml();
      } else {
        context = const SerializationContext.json();
      }
      final serializedBody = bodyReflection.serializeFunction(v, context);
      Object? finalValue = null;
      finalValue = jsonEncode(serializedBody, toEncodable: wireOptions.toEncodable);

      response = await UserApiLoginUserResponse.fromResponse(
        switch (finalValue) {
          String() => createFakeTextResponse(
            statusCode: codeExample,
            value: finalValue,
            contentType: mimeExample.toString(),
          ),
          _ => createFakeTextResponse(
            value: serializedBody.toString(),
            statusCode: codeExample,
            contentType: mimeExample.toString(),
          ),
        },
        userContext: {},
        wireSerializationOptions: wireOptions,
      );
      expect(response.statusCode, codeExample);
      expect(response.headers, containsPair('content-type', mimeExample.toString()));
      //bodyBytesStream SHOULD be null if the response was handled successfully.
      expect(response.bodyBytesStream, OASNetworkingUtils.isMediaTypeSerializable(mimeExample) ? isNull : isNotNull);
    });
    
  });
  group(UserApiLoginUserResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await UserApiLoginUserResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiLoginUserResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Logs out current logged in user session
    //
    // 
    //
    group(r'logoutUser', () {
      group(UserApiLogoutUserRequest, () {
    late UserApiLogoutUserRequest request;
    test(r'No Body', () async {
        request = UserApiLogoutUserRequest(
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

      group(UserApiLogoutUserResponse, () {
  late UserApiLogoutUserResponse response;
  test('Unkown status code', () async {
    response = await UserApiLogoutUserResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<UserApiLogoutUserResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(UserApiLogoutUserResponseDefault, () {
    test('Unknown mime', () async {
      final codeExample = exampleContext.exampleCode(1, 599)
;
      response = await UserApiLogoutUserResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiLogoutUserResponseDefault>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
    // Updated user
    //
    // This can only be done by the logged in user.
    //
    group(r'updateUser', () {
      group(UserApiUpdateUserRequest, () {
    late UserApiUpdateUserRequest request;
    test(UserApiUpdateUserRequestUnsafe, () async {
        request = UserApiUpdateUserRequest.unsafe(
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
            
username: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
.exampleFunction(exampleContext),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(UserApiUpdateUserRequestApplicationJson, () async {
        request = UserApiUpdateUserRequest.applicationJson(
            data: UserApiUpdateUserRequestApplicationJson.dataReflection.exampleFunction(exampleContext),
            extraHeaders: {
                "hello":"world",
            },
            extraQueryParameters: {
                "welcome": "back",
            },
            extraCookies: {
                "nice": "work",
            },
            
username: XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'User',
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
.exampleFunction(exampleContext),

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
});

      group(UserApiUpdateUserResponse, () {
  late UserApiUpdateUserResponse response;
  test('Unkown status code', () async {
    response = await UserApiUpdateUserResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      wireSerializationOptions: wireOptions,
      userContext: {},
    );
    expect(response, isA<UserApiUpdateUserResponse>()
      .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
    );
    expect(response.statusCode, 2);
  });
  group(UserApiUpdateUserResponse400, () {
    test('Unknown mime', () async {
      final codeExample = 
    400
;
      response = await UserApiUpdateUserResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiUpdateUserResponse400>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });
  group(UserApiUpdateUserResponse404, () {
    test('Unknown mime', () async {
      final codeExample = 
    404
;
      response = await UserApiUpdateUserResponse.fromResponse(
        createFakeTextResponse(
          statusCode: codeExample,
          value: "value",
          contentType: "application/whatever",
        ),
        wireSerializationOptions: wireOptions,
        userContext: {},
      );
      expect(response.statusCode, codeExample);
      expect(response, isA<UserApiUpdateUserResponse404>()
        .having((f) => f.bodyBytesStream, 'bodyBytesStream', isNotNull),
      );
    });
    
  });

});
    });
  });
}

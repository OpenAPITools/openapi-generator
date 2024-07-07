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

/// tests for UserApi
void main() {
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
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(UserApiCreateUserResponseDefault, () {
    test('Unkown mime', () async {

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
            data: 


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    User.$reflection.example()
    


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

      group(UserApiCreateUsersWithArrayInputResponse, () {
  late UserApiCreateUsersWithArrayInputResponse response;
  test('Unkown status code', () async {
    response = await UserApiCreateUsersWithArrayInputResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(UserApiCreateUsersWithArrayInputResponseDefault, () {
    test('Unkown mime', () async {

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
            data: 


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    User.$reflection.example()
    


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

      group(UserApiCreateUsersWithListInputResponse, () {
  late UserApiCreateUsersWithListInputResponse response;
  test('Unkown status code', () async {
    response = await UserApiCreateUsersWithListInputResponse.fromResponse(
      createFakeTextResponse(
        statusCode: 2,
        value: "value",
      ),
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(UserApiCreateUsersWithListInputResponseDefault, () {
    test('Unkown mime', () async {

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
            
username:  


            
            


    
    exampleString()


,

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
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(UserApiDeleteUserResponse400, () {
    test('Unkown mime', () async {

    });
    
  });
  group(UserApiDeleteUserResponse404, () {
    test('Unkown mime', () async {

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
            
username:  


            
            


    
    exampleString()


,

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
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(UserApiGetUserByNameResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(UserApiGetUserByNameResponse200ApplicationXml, () async {

    });
    
    test(UserApiGetUserByNameResponse200ApplicationJson, () async {

    });
    
  });
  group(UserApiGetUserByNameResponse400, () {
    test('Unkown mime', () async {

    });
    
  });
  group(UserApiGetUserByNameResponse404, () {
    test('Unkown mime', () async {

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
            
username:  


            
            


    
    exampleString()


,

password:  


            
            


    
    exampleString()


,

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
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(UserApiLoginUserResponse200, () {
    test('Unkown mime', () async {

    });
    
    test(UserApiLoginUserResponse200ApplicationXml, () async {

    });
    
    test(UserApiLoginUserResponse200ApplicationJson, () async {

    });
    
  });
  group(UserApiLoginUserResponse400, () {
    test('Unkown mime', () async {

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
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(UserApiLogoutUserResponseDefault, () {
    test('Unkown mime', () async {

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
            
username:  


            
            


    
    exampleString()


,

        );
        final createdRequest = await request.createHttpRequest(baseUrl: baseUrl);
        expect(createdRequest, isNotNull);
    });
    
    test(UserApiUpdateUserRequestApplicationJson, () async {
        request = UserApiUpdateUserRequest.applicationJson(
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
            
username:  


            
            


    
    exampleString()


,

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
      context: {},
    );
    expect(response.statusCode, 2);
  });
  group(UserApiUpdateUserResponse400, () {
    test('Unkown mime', () async {

    });
    
  });
  group(UserApiUpdateUserResponse404, () {
    test('Unkown mime', () async {

    });
    
  });

});
    });
  });
}

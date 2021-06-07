//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class FakeApi {
  FakeApi([ApiClient apiClient]) : apiClient = apiClient ?? defaultApiClient;

  final ApiClient apiClient;

  /// Health check endpoint
  ///
  /// Note: This method returns the HTTP [Response].
  Future<Response> fakeHealthGetWithHttpInfo() async {
    final path = r'/fake/health';

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Health check endpoint
  Future<HealthCheckResult> fakeHealthGet() async {
    final response = await fakeHealthGetWithHttpInfo();
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'HealthCheckResult',) as HealthCheckResult;
        }
    return Future<HealthCheckResult>.value(null);
  }

  /// test http signature authentication
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  ///
  /// * [String] query1:
  ///   query parameter
  ///
  /// * [String] header1:
  ///   header parameter
  Future<Response> fakeHttpSignatureTestWithHttpInfo(Pet pet, { String query1, String header1 }) async {
    // Verify required params are set.
    if (pet == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: pet');
    }

    final path = r'/fake/http-signature-test';

    Object postBody = pet;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    if (query1 != null) {
      queryParams.addAll(_convertParametersForCollectionFormat('', 'query_1', query1));
    }

    if (header1 != null) {
      headerParams[r'header_1'] = parameterToString(header1);
    }

    final contentTypes = <String>['application/json', 'application/xml'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['http_signature_test'];


    return await apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// test http signature authentication
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  ///
  /// * [String] query1:
  ///   query parameter
  ///
  /// * [String] header1:
  ///   header parameter
  Future<void> fakeHttpSignatureTest(Pet pet, { String query1, String header1 }) async {
    final response = await fakeHttpSignatureTestWithHttpInfo(pet,  query1: query1, header1: header1 );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Test serialization of outer boolean types
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [bool] body:
  ///   Input boolean as post body
  Future<Response> fakeOuterBooleanSerializeWithHttpInfo({ bool body }) async {
    // Verify required params are set.

    final path = r'/fake/outer/boolean';

    Object postBody = body;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Test serialization of outer boolean types
  ///
  /// Parameters:
  ///
  /// * [bool] body:
  ///   Input boolean as post body
  Future<bool> fakeOuterBooleanSerialize({ bool body }) async {
    final response = await fakeOuterBooleanSerializeWithHttpInfo( body: body );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'bool',) as bool;
        }
    return Future<bool>.value(null);
  }

  /// Test serialization of object with outer number type
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [OuterComposite] outerComposite:
  ///   Input composite as post body
  Future<Response> fakeOuterCompositeSerializeWithHttpInfo({ OuterComposite outerComposite }) async {
    // Verify required params are set.

    final path = r'/fake/outer/composite';

    Object postBody = outerComposite;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Test serialization of object with outer number type
  ///
  /// Parameters:
  ///
  /// * [OuterComposite] outerComposite:
  ///   Input composite as post body
  Future<OuterComposite> fakeOuterCompositeSerialize({ OuterComposite outerComposite }) async {
    final response = await fakeOuterCompositeSerializeWithHttpInfo( outerComposite: outerComposite );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'OuterComposite',) as OuterComposite;
        }
    return Future<OuterComposite>.value(null);
  }

  /// Test serialization of outer number types
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [num] body:
  ///   Input number as post body
  Future<Response> fakeOuterNumberSerializeWithHttpInfo({ num body }) async {
    // Verify required params are set.

    final path = r'/fake/outer/number';

    Object postBody = body;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Test serialization of outer number types
  ///
  /// Parameters:
  ///
  /// * [num] body:
  ///   Input number as post body
  Future<num> fakeOuterNumberSerialize({ num body }) async {
    final response = await fakeOuterNumberSerializeWithHttpInfo( body: body );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'num',) as num;
        }
    return Future<num>.value(null);
  }

  /// Test serialization of outer string types
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [String] body:
  ///   Input string as post body
  Future<Response> fakeOuterStringSerializeWithHttpInfo({ String body }) async {
    // Verify required params are set.

    final path = r'/fake/outer/string';

    Object postBody = body;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Test serialization of outer string types
  ///
  /// Parameters:
  ///
  /// * [String] body:
  ///   Input string as post body
  Future<String> fakeOuterStringSerialize({ String body }) async {
    final response = await fakeOuterStringSerializeWithHttpInfo( body: body );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'String',) as String;
        }
    return Future<String>.value(null);
  }

  /// Test serialization of enum (int) properties with examples
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [OuterObjectWithEnumProperty] outerObjectWithEnumProperty (required):
  ///   Input enum (int) as post body
  Future<Response> fakePropertyEnumIntegerSerializeWithHttpInfo(OuterObjectWithEnumProperty outerObjectWithEnumProperty) async {
    // Verify required params are set.
    if (outerObjectWithEnumProperty == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: outerObjectWithEnumProperty');
    }

    final path = r'/fake/property/enum-int';

    Object postBody = outerObjectWithEnumProperty;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Test serialization of enum (int) properties with examples
  ///
  /// Parameters:
  ///
  /// * [OuterObjectWithEnumProperty] outerObjectWithEnumProperty (required):
  ///   Input enum (int) as post body
  Future<OuterObjectWithEnumProperty> fakePropertyEnumIntegerSerialize(OuterObjectWithEnumProperty outerObjectWithEnumProperty) async {
    final response = await fakePropertyEnumIntegerSerializeWithHttpInfo(outerObjectWithEnumProperty);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'OuterObjectWithEnumProperty',) as OuterObjectWithEnumProperty;
        }
    return Future<OuterObjectWithEnumProperty>.value(null);
  }

  /// For this test, the body has to be a binary file.
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [MultipartFile] body (required):
  ///   image to upload
  Future<Response> testBodyWithBinaryWithHttpInfo(MultipartFile body) async {
    // Verify required params are set.
    if (body == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: body');
    }

    final path = r'/fake/body-with-binary';

    Object postBody = body;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['image/png'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'PUT',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// For this test, the body has to be a binary file.
  ///
  /// Parameters:
  ///
  /// * [MultipartFile] body (required):
  ///   image to upload
  Future<void> testBodyWithBinary(MultipartFile body) async {
    final response = await testBodyWithBinaryWithHttpInfo(body);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// For this test, the body for this request must reference a schema named `File`.
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [FileSchemaTestClass] fileSchemaTestClass (required):
  Future<Response> testBodyWithFileSchemaWithHttpInfo(FileSchemaTestClass fileSchemaTestClass) async {
    // Verify required params are set.
    if (fileSchemaTestClass == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: fileSchemaTestClass');
    }

    final path = r'/fake/body-with-file-schema';

    Object postBody = fileSchemaTestClass;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'PUT',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// For this test, the body for this request must reference a schema named `File`.
  ///
  /// Parameters:
  ///
  /// * [FileSchemaTestClass] fileSchemaTestClass (required):
  Future<void> testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass) async {
    final response = await testBodyWithFileSchemaWithHttpInfo(fileSchemaTestClass);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Performs an HTTP 'PUT /fake/body-with-query-params' operation and returns the [Response].
  /// Parameters:
  ///
  /// * [String] query (required):
  ///
  /// * [User] user (required):
  Future<Response> testBodyWithQueryParamsWithHttpInfo(String query, User user) async {
    // Verify required params are set.
    if (query == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: query');
    }
    if (user == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: user');
    }

    final path = r'/fake/body-with-query-params';

    Object postBody = user;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

      queryParams.addAll(_convertParametersForCollectionFormat('', 'query', query));

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'PUT',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Parameters:
  ///
  /// * [String] query (required):
  ///
  /// * [User] user (required):
  Future<void> testBodyWithQueryParams(String query, User user) async {
    final response = await testBodyWithQueryParamsWithHttpInfo(query, user);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// To test \"client\" model
  ///
  /// To test \"client\" model
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [ModelClient] modelClient (required):
  ///   client model
  Future<Response> testClientModelWithHttpInfo(ModelClient modelClient) async {
    // Verify required params are set.
    if (modelClient == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: modelClient');
    }

    final path = r'/fake';

    Object postBody = modelClient;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'PATCH',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// To test \"client\" model
  ///
  /// To test \"client\" model
  ///
  /// Parameters:
  ///
  /// * [ModelClient] modelClient (required):
  ///   client model
  Future<ModelClient> testClientModel(ModelClient modelClient) async {
    final response = await testClientModelWithHttpInfo(modelClient);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'ModelClient',) as ModelClient;
        }
    return Future<ModelClient>.value(null);
  }

  /// Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  ///
  /// Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [num] number (required):
  ///   None
  ///
  /// * [double] double_ (required):
  ///   None
  ///
  /// * [String] patternWithoutDelimiter (required):
  ///   None
  ///
  /// * [String] byte (required):
  ///   None
  ///
  /// * [int] integer:
  ///   None
  ///
  /// * [int] int32:
  ///   None
  ///
  /// * [int] int64:
  ///   None
  ///
  /// * [double] float:
  ///   None
  ///
  /// * [String] string:
  ///   None
  ///
  /// * [MultipartFile] binary:
  ///   None
  ///
  /// * [DateTime] date:
  ///   None
  ///
  /// * [DateTime] dateTime:
  ///   None
  ///
  /// * [String] password:
  ///   None
  ///
  /// * [String] callback:
  ///   None
  Future<Response> testEndpointParametersWithHttpInfo(num number, double double_, String patternWithoutDelimiter, String byte, { int integer, int int32, int int64, double float, String string, MultipartFile binary, DateTime date, DateTime dateTime, String password, String callback }) async {
    // Verify required params are set.
    if (number == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: number');
    }
    if (double_ == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: double_');
    }
    if (patternWithoutDelimiter == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: patternWithoutDelimiter');
    }
    if (byte == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: byte');
    }

    final path = r'/fake';

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/x-www-form-urlencoded'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['http_basic_test'];

    if (integer != null) {
      formParams[r'integer'] = parameterToString(integer);
    }
    if (int32 != null) {
      formParams[r'int32'] = parameterToString(int32);
    }
    if (int64 != null) {
      formParams[r'int64'] = parameterToString(int64);
    }
    if (number != null) {
      formParams[r'number'] = parameterToString(number);
    }
    if (float != null) {
      formParams[r'float'] = parameterToString(float);
    }
    if (double_ != null) {
      formParams[r'double'] = parameterToString(double_);
    }
    if (string != null) {
      formParams[r'string'] = parameterToString(string);
    }
    if (patternWithoutDelimiter != null) {
      formParams[r'pattern_without_delimiter'] = parameterToString(patternWithoutDelimiter);
    }
    if (byte != null) {
      formParams[r'byte'] = parameterToString(byte);
    }
    if (date != null) {
      formParams[r'date'] = parameterToString(date);
    }
    if (dateTime != null) {
      formParams[r'dateTime'] = parameterToString(dateTime);
    }
    if (password != null) {
      formParams[r'password'] = parameterToString(password);
    }
    if (callback != null) {
      formParams[r'callback'] = parameterToString(callback);
    }

    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  ///
  /// Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  ///
  /// Parameters:
  ///
  /// * [num] number (required):
  ///   None
  ///
  /// * [double] double_ (required):
  ///   None
  ///
  /// * [String] patternWithoutDelimiter (required):
  ///   None
  ///
  /// * [String] byte (required):
  ///   None
  ///
  /// * [int] integer:
  ///   None
  ///
  /// * [int] int32:
  ///   None
  ///
  /// * [int] int64:
  ///   None
  ///
  /// * [double] float:
  ///   None
  ///
  /// * [String] string:
  ///   None
  ///
  /// * [MultipartFile] binary:
  ///   None
  ///
  /// * [DateTime] date:
  ///   None
  ///
  /// * [DateTime] dateTime:
  ///   None
  ///
  /// * [String] password:
  ///   None
  ///
  /// * [String] callback:
  ///   None
  Future<void> testEndpointParameters(num number, double double_, String patternWithoutDelimiter, String byte, { int integer, int int32, int int64, double float, String string, MultipartFile binary, DateTime date, DateTime dateTime, String password, String callback }) async {
    final response = await testEndpointParametersWithHttpInfo(number, double_, patternWithoutDelimiter, byte,  integer: integer, int32: int32, int64: int64, float: float, string: string, binary: binary, date: date, dateTime: dateTime, password: password, callback: callback );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// To test enum parameters
  ///
  /// To test enum parameters
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [List<String>] enumHeaderStringArray:
  ///   Header parameter enum test (string array)
  ///
  /// * [String] enumHeaderString:
  ///   Header parameter enum test (string)
  ///
  /// * [List<String>] enumQueryStringArray:
  ///   Query parameter enum test (string array)
  ///
  /// * [String] enumQueryString:
  ///   Query parameter enum test (string)
  ///
  /// * [int] enumQueryInteger:
  ///   Query parameter enum test (double)
  ///
  /// * [double] enumQueryDouble:
  ///   Query parameter enum test (double)
  ///
  /// * [List<String>] enumFormStringArray:
  ///   Form parameter enum test (string array)
  ///
  /// * [String] enumFormString:
  ///   Form parameter enum test (string)
  Future<Response> testEnumParametersWithHttpInfo({ List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, int enumQueryInteger, double enumQueryDouble, List<String> enumFormStringArray, String enumFormString }) async {
    // Verify required params are set.

    final path = r'/fake';

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    if (enumQueryStringArray != null) {
      queryParams.addAll(_convertParametersForCollectionFormat('multi', 'enum_query_string_array', enumQueryStringArray));
    }
    if (enumQueryString != null) {
      queryParams.addAll(_convertParametersForCollectionFormat('', 'enum_query_string', enumQueryString));
    }
    if (enumQueryInteger != null) {
      queryParams.addAll(_convertParametersForCollectionFormat('', 'enum_query_integer', enumQueryInteger));
    }
    if (enumQueryDouble != null) {
      queryParams.addAll(_convertParametersForCollectionFormat('', 'enum_query_double', enumQueryDouble));
    }

    if (enumHeaderStringArray != null) {
      headerParams[r'enum_header_string_array'] = parameterToString(enumHeaderStringArray);
    }
    if (enumHeaderString != null) {
      headerParams[r'enum_header_string'] = parameterToString(enumHeaderString);
    }

    final contentTypes = <String>['application/x-www-form-urlencoded'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];

    if (enumFormStringArray != null) {
      formParams[r'enum_form_string_array'] = parameterToString(enumFormStringArray);
    }
    if (enumFormString != null) {
      formParams[r'enum_form_string'] = parameterToString(enumFormString);
    }

    return await apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// To test enum parameters
  ///
  /// To test enum parameters
  ///
  /// Parameters:
  ///
  /// * [List<String>] enumHeaderStringArray:
  ///   Header parameter enum test (string array)
  ///
  /// * [String] enumHeaderString:
  ///   Header parameter enum test (string)
  ///
  /// * [List<String>] enumQueryStringArray:
  ///   Query parameter enum test (string array)
  ///
  /// * [String] enumQueryString:
  ///   Query parameter enum test (string)
  ///
  /// * [int] enumQueryInteger:
  ///   Query parameter enum test (double)
  ///
  /// * [double] enumQueryDouble:
  ///   Query parameter enum test (double)
  ///
  /// * [List<String>] enumFormStringArray:
  ///   Form parameter enum test (string array)
  ///
  /// * [String] enumFormString:
  ///   Form parameter enum test (string)
  Future<void> testEnumParameters({ List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, int enumQueryInteger, double enumQueryDouble, List<String> enumFormStringArray, String enumFormString }) async {
    final response = await testEnumParametersWithHttpInfo( enumHeaderStringArray: enumHeaderStringArray, enumHeaderString: enumHeaderString, enumQueryStringArray: enumQueryStringArray, enumQueryString: enumQueryString, enumQueryInteger: enumQueryInteger, enumQueryDouble: enumQueryDouble, enumFormStringArray: enumFormStringArray, enumFormString: enumFormString );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Fake endpoint to test group parameters (optional)
  ///
  /// Fake endpoint to test group parameters (optional)
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [int] requiredStringGroup (required):
  ///   Required String in group parameters
  ///
  /// * [bool] requiredBooleanGroup (required):
  ///   Required Boolean in group parameters
  ///
  /// * [int] requiredInt64Group (required):
  ///   Required Integer in group parameters
  ///
  /// * [int] stringGroup:
  ///   String in group parameters
  ///
  /// * [bool] booleanGroup:
  ///   Boolean in group parameters
  ///
  /// * [int] int64Group:
  ///   Integer in group parameters
  Future<Response> testGroupParametersWithHttpInfo(int requiredStringGroup, bool requiredBooleanGroup, int requiredInt64Group, { int stringGroup, bool booleanGroup, int int64Group }) async {
    // Verify required params are set.
    if (requiredStringGroup == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: requiredStringGroup');
    }
    if (requiredBooleanGroup == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: requiredBooleanGroup');
    }
    if (requiredInt64Group == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: requiredInt64Group');
    }

    final path = r'/fake';

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

      queryParams.addAll(_convertParametersForCollectionFormat('', 'required_string_group', requiredStringGroup));
      queryParams.addAll(_convertParametersForCollectionFormat('', 'required_int64_group', requiredInt64Group));
    if (stringGroup != null) {
      queryParams.addAll(_convertParametersForCollectionFormat('', 'string_group', stringGroup));
    }
    if (int64Group != null) {
      queryParams.addAll(_convertParametersForCollectionFormat('', 'int64_group', int64Group));
    }

    headerParams[r'required_boolean_group'] = parameterToString(requiredBooleanGroup);
    if (booleanGroup != null) {
      headerParams[r'boolean_group'] = parameterToString(booleanGroup);
    }

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['bearer_test'];


    return await apiClient.invokeAPI(
      path,
      'DELETE',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Fake endpoint to test group parameters (optional)
  ///
  /// Fake endpoint to test group parameters (optional)
  ///
  /// Parameters:
  ///
  /// * [int] requiredStringGroup (required):
  ///   Required String in group parameters
  ///
  /// * [bool] requiredBooleanGroup (required):
  ///   Required Boolean in group parameters
  ///
  /// * [int] requiredInt64Group (required):
  ///   Required Integer in group parameters
  ///
  /// * [int] stringGroup:
  ///   String in group parameters
  ///
  /// * [bool] booleanGroup:
  ///   Boolean in group parameters
  ///
  /// * [int] int64Group:
  ///   Integer in group parameters
  Future<void> testGroupParameters(int requiredStringGroup, bool requiredBooleanGroup, int requiredInt64Group, { int stringGroup, bool booleanGroup, int int64Group }) async {
    final response = await testGroupParametersWithHttpInfo(requiredStringGroup, requiredBooleanGroup, requiredInt64Group,  stringGroup: stringGroup, booleanGroup: booleanGroup, int64Group: int64Group );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// test inline additionalProperties
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [Map<String, String>] requestBody (required):
  ///   request body
  Future<Response> testInlineAdditionalPropertiesWithHttpInfo(Map<String, String> requestBody) async {
    // Verify required params are set.
    if (requestBody == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: requestBody');
    }

    final path = r'/fake/inline-additionalProperties';

    Object postBody = requestBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// test inline additionalProperties
  ///
  /// Parameters:
  ///
  /// * [Map<String, String>] requestBody (required):
  ///   request body
  Future<void> testInlineAdditionalProperties(Map<String, String> requestBody) async {
    final response = await testInlineAdditionalPropertiesWithHttpInfo(requestBody);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// test json serialization of form data
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [String] param (required):
  ///   field1
  ///
  /// * [String] param2 (required):
  ///   field2
  Future<Response> testJsonFormDataWithHttpInfo(String param, String param2) async {
    // Verify required params are set.
    if (param == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: param');
    }
    if (param2 == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: param2');
    }

    final path = r'/fake/jsonFormData';

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/x-www-form-urlencoded'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];

    if (param != null) {
      formParams[r'param'] = parameterToString(param);
    }
    if (param2 != null) {
      formParams[r'param2'] = parameterToString(param2);
    }

    return await apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// test json serialization of form data
  ///
  /// Parameters:
  ///
  /// * [String] param (required):
  ///   field1
  ///
  /// * [String] param2 (required):
  ///   field2
  Future<void> testJsonFormData(String param, String param2) async {
    final response = await testJsonFormDataWithHttpInfo(param, param2);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// To test the collection format in query parameters
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [List<String>] pipe (required):
  ///
  /// * [List<String>] ioutil (required):
  ///
  /// * [List<String>] http (required):
  ///
  /// * [List<String>] url (required):
  ///
  /// * [List<String>] context (required):
  Future<Response> testQueryParameterCollectionFormatWithHttpInfo(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context) async {
    // Verify required params are set.
    if (pipe == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: pipe');
    }
    if (ioutil == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: ioutil');
    }
    if (http == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: http');
    }
    if (url == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: url');
    }
    if (context == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: context');
    }

    final path = r'/fake/test-query-paramters';

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

      queryParams.addAll(_convertParametersForCollectionFormat('pipes', 'pipe', pipe));
      queryParams.addAll(_convertParametersForCollectionFormat('csv', 'ioutil', ioutil));
      queryParams.addAll(_convertParametersForCollectionFormat('ssv', 'http', http));
      queryParams.addAll(_convertParametersForCollectionFormat('csv', 'url', url));
      queryParams.addAll(_convertParametersForCollectionFormat('multi', 'context', context));

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];


    return await apiClient.invokeAPI(
      path,
      'PUT',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// To test the collection format in query parameters
  ///
  /// Parameters:
  ///
  /// * [List<String>] pipe (required):
  ///
  /// * [List<String>] ioutil (required):
  ///
  /// * [List<String>] http (required):
  ///
  /// * [List<String>] url (required):
  ///
  /// * [List<String>] context (required):
  Future<void> testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context) async {
    final response = await testQueryParameterCollectionFormatWithHttpInfo(pipe, ioutil, http, url, context);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }
}

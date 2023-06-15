//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'dart:async';
import 'package:dio/dio.dart';
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
import 'package:openapi/src/repository_base.dart';
import 'package:openapi/src/api_util.dart';
import 'package:openapi/models.dart';
import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/api_response.dart';
=======

import 'package:openapi/src/model/fake_big_decimal_map200_response.dart';
import 'package:openapi/src/model/file_schema_test_class.dart';
import 'package:openapi/src/model/health_check_result.dart';
import 'package:openapi/src/model/model_client.dart';
import 'package:openapi/src/model/model_enum_class.dart';
import 'package:openapi/src/model/outer_composite.dart';
import 'package:openapi/src/model/outer_object_with_enum_property.dart';
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
import 'package:openapi/src/model/pet.dart';

class PetApi {
  final PetApiRaw rawApi;
  final SerializationRepositoryBase _repository;

  const PetApi(this.rawApi, this._repository);

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Add a new pet to the store
  ///
=======
  /// fakeBigDecimalMap
  /// for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future] containing a [Response] with a [FakeBigDecimalMap200Response] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<FakeBigDecimalMap200Response>> fakeBigDecimalMap({ 
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/BigDecimalMap';
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      validateStatus: validateStatus,
    );

    final _response = await _dio.request<Object>(
      _path,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    FakeBigDecimalMap200Response? _responseData;

    try {
final rawData = _response.data;
_responseData = rawData == null ? null : deserialize<FakeBigDecimalMap200Response, FakeBigDecimalMap200Response>(rawData, 'FakeBigDecimalMap200Response', growable: true);
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<FakeBigDecimalMap200Response>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// Health check endpoint
  /// 
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
  ///
  /// Parameters:
  /// * [pet] - Pet object that needs to be added to the store
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> addPet({
    required Pet pet,
=======
  /// Returns a [Future] containing a [Response] with a [HealthCheckResult] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<HealthCheckResult>> fakeHealthGet({ 
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    Object? _bodyData;
    _bodyData = _repository.serialize(pet, const TypeInfo(Pet));

    final _response = await rawApi.addPet(
      body: _bodyData,
      requestContentType: 'application/json',
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    return _response;
=======
    HealthCheckResult? _responseData;

    try {
final rawData = _response.data;
_responseData = rawData == null ? null : deserialize<HealthCheckResult, HealthCheckResult>(rawData, 'HealthCheckResult', growable: true);
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<HealthCheckResult>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
  }

  /// Deletes a pet
  ///
  ///
  /// Parameters:
  /// * [petId] - Pet id to delete
  /// * [apiKey]
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> deletePet({
    required int petId,
    String? apiKey,
=======
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> fakeHttpSignatureTest({ 
    required Pet pet,
    String? query1,
    String? header1,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    final _response = await rawApi.deletePet(
      petId: _repository.serialize(petId, const TypeInfo(int)).toString(),
      apiKey: apiKey == null
          ? null
          : _repository.serialize(apiKey, const TypeInfo(String)).toString(),
=======
    final _path = r'/fake/http-signature-test';
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (header1 != null) r'header_1': header1,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'http',
            'scheme': 'signature',
            'name': 'http_signature_test',
          },
        ],
        ...?extra,
      },
      contentType: 'application/json',
      validateStatus: validateStatus,
    );

    final _queryParameters = <String, dynamic>{
      if (query1 != null) r'query_1': query1,
    };

    dynamic _bodyData;

    try {
_bodyData=jsonEncode(pet);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
          queryParameters: _queryParameters,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
      queryParameters: _queryParameters,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

  /// Finds Pets by status
  /// Multiple status values can be provided with comma separated strings
  ///
  /// Parameters:
  /// * [status] - Status values that need to be considered for filter
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Returns a [Future] containing a [Response] with a [BuiltList<Pet>] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<BuiltList<Pet>>> findPetsByStatus({
    @Deprecated('status is deprecated') required BuiltList<String> status,
=======
  /// Returns a [Future] containing a [Response] with a [bool] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<bool>> fakeOuterBooleanSerialize({ 
    bool? body,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    final _response = await rawApi.findPetsByStatus(
      status: encodeQueryParameter(
        _repository,
        status,
        const TypeInfo(BuiltList, [
          const TypeInfo(String),
        ]),
        format: ListFormat.csv,
      ),
=======
    final _path = r'/fake/outer/boolean';
    final _options = Options(
      method: r'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: 'application/json',
      validateStatus: validateStatus,
    );

    dynamic _bodyData;

    try {
_bodyData=jsonEncode(body);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    BuiltList<Pet>? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(BuiltList, [
                const TypeInfo(Pet),
              ]),
            );
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<BuiltList<Pet>>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// Finds Pets by tags
  /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
  ///
  /// Parameters:
  /// * [tags] - Tags to filter by
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Returns a [Future] containing a [Response] with a [BuiltSet<Pet>] as data
  /// Throws [DioError] if API call or serialization fails
  @Deprecated('This operation has been deprecated')
  Future<Response<BuiltSet<Pet>>> findPetsByTags({
    required BuiltSet<String> tags,
=======
  /// Returns a [Future] containing a [Response] with a [OuterComposite] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<OuterComposite>> fakeOuterCompositeSerialize({ 
    OuterComposite? outerComposite,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    final _response = await rawApi.findPetsByTags(
      tags: encodeQueryParameter(
        _repository,
        tags,
        const TypeInfo(BuiltSet, [
          const TypeInfo(String),
        ]),
        format: ListFormat.csv,
      ),
=======
    final _path = r'/fake/outer/composite';
    final _options = Options(
      method: r'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: 'application/json',
      validateStatus: validateStatus,
    );

    dynamic _bodyData;

    try {
_bodyData=jsonEncode(outerComposite);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    BuiltSet<Pet>? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(BuiltSet, [
                const TypeInfo(Pet),
              ]),
            );
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<BuiltSet<Pet>>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// Find pet by ID
  /// Returns a single pet
  ///
  /// Parameters:
  /// * [petId] - ID of pet to return
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Returns a [Future] containing a [Response] with a [Pet] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Pet>> getPetById({
    required int petId,
=======
  /// Returns a [Future] containing a [Response] with a [num] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<num>> fakeOuterNumberSerialize({ 
    num? body,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    final _response = await rawApi.getPetById(
      petId: _repository.serialize(petId, const TypeInfo(int)).toString(),
=======
    final _path = r'/fake/outer/number';
    final _options = Options(
      method: r'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: 'application/json',
      validateStatus: validateStatus,
    );

    dynamic _bodyData;

    try {
_bodyData=jsonEncode(body);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    final _response = await _dio.request<Object>(
      _path,
      data: _bodyData,
      options: _options,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    Pet? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(Pet),
            );
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<Pet>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// Update an existing pet
  ///
  ///
  /// Parameters:
  /// * [pet] - Pet object that needs to be added to the store
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> updatePet({
    required Pet pet,
=======
  /// Returns a [Future] containing a [Response] with a [String] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<String>> fakeOuterStringSerialize({ 
    String? body,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    Object? _bodyData;
    _bodyData = _repository.serialize(pet, const TypeInfo(Pet));
=======
    final _path = r'/fake/outer/string';
    final _options = Options(
      method: r'POST',
      headers: <String, dynamic>{
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: 'application/json',
      validateStatus: validateStatus,
    );

    dynamic _bodyData;

    try {
_bodyData=jsonEncode(body);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart

    final _response = await rawApi.updatePet(
      body: _bodyData,
      requestContentType: 'application/json',
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    return _response;
=======
    String? _responseData;

    try {
final rawData = _response.data;
_responseData = rawData == null ? null : deserialize<String, String>(rawData, 'String', growable: true);
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<String>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
  }

  /// Updates a pet in the store with form data
  ///
  ///
  /// Parameters:
  /// * [petId] - ID of pet that needs to be updated
  /// * [name] - Updated name of the pet
  /// * [status] - Updated status of the pet
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> updatePetWithForm({
    required int petId,
    String? name,
    String? status,
=======
  /// Returns a [Future] containing a [Response] with a [OuterObjectWithEnumProperty] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<OuterObjectWithEnumProperty>> fakePropertyEnumIntegerSerialize({ 
    required OuterObjectWithEnumProperty outerObjectWithEnumProperty,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    Object? _bodyData;
    _bodyData = <String, dynamic>{
      if (name != null)
        r'name': encodeFormParameter(_repository, name, const TypeInfo(String)),
      if (status != null)
        r'status':
            encodeFormParameter(_repository, status, const TypeInfo(String)),
    };

    final _response = await rawApi.updatePetWithForm(
      petId: _repository.serialize(petId, const TypeInfo(int)).toString(),
      body: _bodyData,
      requestContentType: 'application/x-www-form-urlencoded',
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    return _response;
  }
=======
    dynamic _bodyData;

    try {
_bodyData=jsonEncode(outerObjectWithEnumProperty);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart

  /// uploads an image
  ///
  ///
  /// Parameters:
  /// * [petId] - ID of pet to update
  /// * [additionalMetadata] - Additional data to pass to server
  /// * [file] - file to upload
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future] containing a [Response] with a [ApiResponse] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<ApiResponse>> uploadFile({
    required int petId,
    String? additionalMetadata,
    MultipartFile? file,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    Object? _bodyData;
    _bodyData = FormData.fromMap(<String, dynamic>{
      if (additionalMetadata != null)
        r'additionalMetadata': encodeFormParameter(
            _repository, additionalMetadata, const TypeInfo(String)),
      if (file != null) r'file': file,
    });

    final _response = await rawApi.uploadFile(
      petId: _repository.serialize(petId, const TypeInfo(int)).toString(),
      body: _bodyData,
      requestContentType: 'multipart/form-data',
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    ApiResponse? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(ApiResponse),
            );
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<ApiResponse>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }

  /// uploads an image (required)
  ///
  ///
  /// Parameters:
  /// * [petId] - ID of pet to update
  /// * [requiredFile] - file to upload
  /// * [additionalMetadata] - Additional data to pass to server
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Returns a [Future] containing a [Response] with a [ApiResponse] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<ApiResponse>> uploadFileWithRequiredFile({
    required int petId,
    required MultipartFile requiredFile,
    String? additionalMetadata,
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> testBodyWithBinary({ 
    MultipartFile? body,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    Object? _bodyData;
    _bodyData = FormData.fromMap(<String, dynamic>{
      if (additionalMetadata != null)
        r'additionalMetadata': encodeFormParameter(
            _repository, additionalMetadata, const TypeInfo(String)),
      r'requiredFile': requiredFile,
    });

    final _response = await rawApi.uploadFileWithRequiredFile(
      petId: _repository.serialize(petId, const TypeInfo(int)).toString(),
      body: _bodyData,
      requestContentType: 'multipart/form-data',
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    ApiResponse? _responseData;

    try {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(ApiResponse),
            );
    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.unknown,
=======
_bodyData=jsonEncode(body);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<ApiResponse>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
  }
}

class PetApiRaw {
  final Dio _dio;

  const PetApiRaw(this._dio);

  /// Add a new pet to the store
  ///
  ///
  /// Parameters:
  /// * [pet] - Pet object that needs to be added to the store
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> addPet({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> testBodyWithFileSchema({ 
    required FileSchemaTestClass fileSchemaTestClass,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/pet';
    final _options = Options(
      method: r'POST',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'oauth2',
            'name': 'petstore_auth',
          },
        ],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    return await _dio.request<Object>(
=======
    dynamic _bodyData;

    try {
_bodyData=jsonEncode(fileSchemaTestClass);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    final _response = await _dio.request<Object>(
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

  /// Deletes a pet
  ///
  ///
  /// Parameters:
  /// * [petId] - Pet id to delete
  /// * [apiKey]
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> deletePet({
    required String petId,
    String? apiKey,
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> testBodyWithQueryParams({ 
    required String query,
    required User user,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path =
        r'/pet/{petId}'.replaceAll('{' r'petId' '}', petId.toString());
    final _options = Options(
      method: r'DELETE',
      headers: <String, dynamic>{
        if (apiKey != null) r'api_key': apiKey,
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'oauth2',
            'name': 'petstore_auth',
          },
        ],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    return await _dio.request<Object>(
=======
    final _queryParameters = <String, dynamic>{
      r'query': query,
    };

    dynamic _bodyData;

    try {
_bodyData=jsonEncode(user);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
          queryParameters: _queryParameters,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    final _response = await _dio.request<Object>(
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

  /// Finds Pets by status
  /// Multiple status values can be provided with comma separated strings
  ///
  /// Parameters:
  /// * [status] - Status values that need to be considered for filter
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Returns a [Future] containing a [Response] with a [BuiltList<Pet>] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> findPetsByStatus({
    @Deprecated('status is deprecated') required Object status,
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future] containing a [Response] with a [ModelClient] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<ModelClient>> testClientModel({ 
    required ModelClient modelClient,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/pet/findByStatus';
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'oauth2',
            'name': 'petstore_auth',
          },
        ],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    final _queryParameters = <String, dynamic>{
      r'status': status,
    };
=======
    dynamic _bodyData;

    try {
_bodyData=jsonEncode(modelClient);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart

    return await _dio.request<Object>(
      _path,
      data: body,
      options: _options,
      queryParameters: _queryParameters,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
=======

    ModelClient? _responseData;

    try {
final rawData = _response.data;
_responseData = rawData == null ? null : deserialize<ModelClient, ModelClient>(rawData, 'ModelClient', growable: true);
    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<ModelClient>(
      data: _responseData,
      headers: _response.headers,
      isRedirect: _response.isRedirect,
      requestOptions: _response.requestOptions,
      redirects: _response.redirects,
      statusCode: _response.statusCode,
      statusMessage: _response.statusMessage,
      extra: _response.extra,
    );
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
  }

  /// Finds Pets by tags
  /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
  ///
  /// Parameters:
  /// * [tags] - Tags to filter by
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Returns a [Future] containing a [Response] with a [BuiltSet<Pet>] as data
  /// Throws [DioError] if API call or serialization fails
  @Deprecated('This operation has been deprecated')
  Future<Response<Object>> findPetsByTags({
    required Object tags,
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> testEndpointParameters({ 
    required num number,
    required double double_,
    required String patternWithoutDelimiter,
    required String byte,
    int? integer,
    int? int32,
    int? int64,
    double? float,
    String? string,
    MultipartFile? binary,
    DateTime? date,
    DateTime? dateTime,
    String? password,
    String? callback,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/pet/findByTags';
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'oauth2',
            'name': 'petstore_auth',
          },
        ],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    final _queryParameters = <String, dynamic>{
      r'tags': tags,
    };
=======
    dynamic _bodyData;

    try {

    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart

    return await _dio.request<Object>(
      _path,
      data: body,
      options: _options,
      queryParameters: _queryParameters,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

  /// Find pet by ID
  /// Returns a single pet
  ///
  /// Parameters:
  /// * [petId] - ID of pet to return
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Returns a [Future] containing a [Response] with a [Pet] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> getPetById({
    required String petId,
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> testEnumParameters({ 
    List<String>? enumHeaderStringArray,
    String? enumHeaderString = '-efg',
    List<String>? enumQueryStringArray,
    String? enumQueryString = '-efg',
    int? enumQueryInteger,
    double? enumQueryDouble,
    List<ModelEnumClass>? enumQueryModelArray,
    List<String>? enumFormStringArray,
    String? enumFormString,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path =
        r'/pet/{petId}'.replaceAll('{' r'petId' '}', petId.toString());
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'apiKey',
            'name': 'api_key',
            'keyName': 'api_key',
            'where': 'header',
          },
        ],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    return await _dio.request<Object>(
=======
    final _queryParameters = <String, dynamic>{
      if (enumQueryStringArray != null) r'enum_query_string_array': enumQueryStringArray,
      if (enumQueryString != null) r'enum_query_string': enumQueryString,
      if (enumQueryInteger != null) r'enum_query_integer': enumQueryInteger,
      if (enumQueryDouble != null) r'enum_query_double': enumQueryDouble,
      if (enumQueryModelArray != null) r'enum_query_model_array': enumQueryModelArray,
    };

    dynamic _bodyData;

    try {

    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
          queryParameters: _queryParameters,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    final _response = await _dio.request<Object>(
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

  /// Update an existing pet
  ///
  ///
  /// Parameters:
  /// * [pet] - Pet object that needs to be added to the store
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> updatePet({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> testGroupParameters({ 
    required int requiredStringGroup,
    required bool requiredBooleanGroup,
    required int requiredInt64Group,
    int? stringGroup,
    bool? booleanGroup,
    int? int64Group,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/pet';
    final _options = Options(
      method: r'PUT',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'oauth2',
            'name': 'petstore_auth',
          },
        ],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

    return await _dio.request<Object>(
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

  /// Updates a pet in the store with form data
  ///
  ///
  /// Parameters:
  /// * [petId] - ID of pet that needs to be updated
  /// * [name] - Updated name of the pet
  /// * [status] - Updated status of the pet
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> updatePetWithForm({
    required String petId,
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> testInlineAdditionalProperties({ 
    required Map<String, String> requestBody,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path =
        r'/pet/{petId}'.replaceAll('{' r'petId' '}', petId.toString());
    final _options = Options(
      method: r'POST',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'oauth2',
            'name': 'petstore_auth',
          },
        ],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    return await _dio.request<Object>(
=======
    dynamic _bodyData;

    try {
_bodyData=jsonEncode(requestBody);
    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    final _response = await _dio.request<Object>(
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

  /// uploads an image
  ///
  ///
  /// Parameters:
  /// * [petId] - ID of pet to update
  /// * [additionalMetadata] - Additional data to pass to server
  /// * [file] - file to upload
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Returns a [Future] containing a [Response] with a [ApiResponse] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> uploadFile({
    required String petId,
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> testJsonFormData({ 
    required String param,
    required String param2,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/pet/{petId}/uploadImage'
        .replaceAll('{' r'petId' '}', petId.toString());
    final _options = Options(
      method: r'POST',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'oauth2',
            'name': 'petstore_auth',
          },
        ],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
    return await _dio.request<Object>(
=======
    dynamic _bodyData;

    try {

    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    final _response = await _dio.request<Object>(
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

  /// uploads an image (required)
  ///
  ///
  /// Parameters:
  /// * [petId] - ID of pet to update
  /// * [requiredFile] - file to upload
  /// * [additionalMetadata] - Additional data to pass to server
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/pet_api.dart
  /// Returns a [Future] containing a [Response] with a [ApiResponse] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> uploadFileWithRequiredFile({
    required String petId,
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> testQueryParameterCollectionFormat({ 
    required List<String> pipe,
    required List<String> ioutil,
    required List<String> http,
    required List<String> url,
    required List<String> context,
    required String allowEmpty,
    Map<String, String>? language,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake-json_serializable/lib/src/api/fake_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/fake/{petId}/uploadImageWithRequiredFile'
        .replaceAll('{' r'petId' '}', petId.toString());
    final _options = Options(
      method: r'POST',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[
          {
            'type': 'oauth2',
            'name': 'petstore_auth',
          },
        ],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

    return await _dio.request<Object>(
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }
}

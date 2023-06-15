//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'dart:async';
import 'package:dio/dio.dart';
import 'package:openapi/src/repository_base.dart';
import 'package:openapi/src/api_util.dart';
import 'package:openapi/models.dart';
import 'package:built_value/json_object.dart';
import 'package:openapi/src/model/foo_basic_get_default_response.dart';
import 'package:openapi/src/model/fruit.dart';
import 'package:openapi/src/model/fruit_all_of_disc.dart';
import 'package:openapi/src/model/fruit_variant1.dart';
import 'package:openapi/src/model/giga_one_of.dart';

class DefaultApi {
  final DefaultApiRaw rawApi;
  final SerializationRepositoryBase _repository;

  const DefaultApi(this.rawApi, this._repository);

  /// fooBasicGet
  ///
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
  /// Returns a [Future] containing a [Response] with a [FooBasicGetDefaultResponse] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<FooBasicGetDefaultResponse>> fooBasicGet({
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> addPet({ 
    required Pet pet,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _response = await rawApi.fooBasicGet(
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    FooBasicGetDefaultResponse? _responseData;

    try {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(FooBasicGetDefaultResponse),
            );
    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.unknown,
=======
      const _type = FullType(Pet);
      _bodyData = _serializers.serialize(pet, specifiedType: _type);

    } catch(error, stackTrace) {
      throw DioException(
         requestOptions: _options.compose(
          _dio.options,
          _path,
        ),
        type: DioExceptionType.unknown,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<FooBasicGetDefaultResponse>(
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

  /// list
  ///
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
  /// Returns a [Future] containing a [Response] with a [GigaOneOf] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<GigaOneOf>> list({
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> deletePet({ 
    required int petId,
    String? apiKey,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
    final _response = await rawApi.list(
=======
    final _path = r'/pet/{petId}'.replaceAll('{' r'petId' '}', encodeQueryParameter(_serializers, petId, const FullType(int)).toString());
    final _options = Options(
      method: r'DELETE',
      headers: <String, dynamic>{
        if (apiKey != null) r'api_key': apiKey,
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
      validateStatus: validateStatus,
    );

    final _response = await _dio.request<Object>(
      _path,
      options: _options,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    GigaOneOf? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(GigaOneOf),
            );
    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }

    return Response<GigaOneOf>(
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

  /// oneofGet
  ///
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
  /// Returns a [Future] containing a [Response] with a [Fruit] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Fruit>> oneofGet({
=======
  /// Returns a [Future] containing a [Response] with a [BuiltList<Pet>] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<BuiltList<Pet>>> findPetsByStatus({ 
    @Deprecated('status is deprecated') required BuiltList<String> status,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _response = await rawApi.oneofGet(
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    Fruit? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(Fruit),
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

    return Response<Fruit>(
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

  /// test
  ///
  ///
  /// Parameters:
  /// * [body]
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> test({
    JsonObject? body,
=======
  /// Returns a [Future] containing a [Response] with a [BuiltSet<Pet>] as data
  /// Throws [DioException] if API call or serialization fails
  @Deprecated('This operation has been deprecated')
  Future<Response<BuiltSet<Pet>>> findPetsByTags({ 
    required BuiltSet<String> tags,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    Object? _bodyData;
    _bodyData = body;

    final _response = await rawApi.test(
      body: _bodyData,
      requestContentType: 'application/json',
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    return _response;
  }

  /// variant1Get
  ///
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future] containing a [Response] with a [FruitVariant1] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<FruitVariant1>> variant1Get({
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _response = await rawApi.variant1Get(
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    FruitVariant1? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(FruitVariant1),
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

    return Response<FruitVariant1>(
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

  /// variant2Get
  ///
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
  /// Returns a [Future] containing a [Response] with a [FruitAllOfDisc] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<FruitAllOfDisc>> variant2Get({
=======
  /// Returns a [Future] containing a [Response] with a [Pet] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<Pet>> getPetById({ 
    required int petId,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
    final _response = await rawApi.variant2Get(
=======
    final _path = r'/pet/{petId}'.replaceAll('{' r'petId' '}', encodeQueryParameter(_serializers, petId, const FullType(int)).toString());
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
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
      validateStatus: validateStatus,
    );

    final _response = await _dio.request<Object>(
      _path,
      options: _options,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
      cancelToken: cancelToken,
      headers: headers,
      extra: extra,
      validateStatus: validateStatus,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );

    FruitAllOfDisc? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null
          ? null
          : _repository.deserialize(
              rawResponse,
              const TypeInfo(FruitAllOfDisc),
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

    return Response<FruitAllOfDisc>(
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

class DefaultApiRaw {
  final Dio _dio;

  const DefaultApiRaw(this._dio);

  /// fooBasicGet
  ///
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
  /// Returns a [Future] containing a [Response] with a [FooBasicGetDefaultResponse] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> fooBasicGet({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> updatePet({ 
    required Pet pet,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/foo-basic';
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
    return await _dio.request<Object>(
=======
    dynamic _bodyData;

    try {
      const _type = FullType(Pet);
      _bodyData = _serializers.serialize(pet, specifiedType: _type);

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
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

  /// list
  ///
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
  /// Returns a [Future] containing a [Response] with a [GigaOneOf] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> list({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future]
  /// Throws [DioException] if API call or serialization fails
  Future<Response<void>> updatePetWithForm({ 
    required int petId,
    String? name,
    String? status,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
    final _path = r'/oneof-primitive';
=======
    final _path = r'/pet/{petId}'.replaceAll('{' r'petId' '}', encodeQueryParameter(_serializers, petId, const FullType(int)).toString());
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
    return await _dio.request<Object>(
=======
    dynamic _bodyData;

    try {
      _bodyData = <String, dynamic>{
        if (name != null) r'name': encodeQueryParameter(_serializers, name, const FullType(String)),
        if (status != null) r'status': encodeQueryParameter(_serializers, status, const FullType(String)),
      };

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
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

  /// oneofGet
  ///
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
  /// Returns a [Future] containing a [Response] with a [Fruit] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> oneofGet({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future] containing a [Response] with a [ApiResponse] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<ApiResponse>> uploadFile({ 
    required int petId,
    String? additionalMetadata,
    MultipartFile? file,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
    final _path = r'/oneof';
=======
    final _path = r'/pet/{petId}/uploadImage'.replaceAll('{' r'petId' '}', encodeQueryParameter(_serializers, petId, const FullType(int)).toString());
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
    return await _dio.request<Object>(
=======
    dynamic _bodyData;

    try {
      _bodyData = FormData.fromMap(<String, dynamic>{
        if (additionalMetadata != null) r'additionalMetadata': encodeFormParameter(_serializers, additionalMetadata, const FullType(String)),
        if (file != null) r'file': file,
      });

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
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
  /// test
  ///
  ///
  /// Parameters:
  /// * [body]
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future]
  /// Throws [DioError] if API call or serialization fails
  Future<Response<void>> test({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/variant1';
    final _options = Options(
      method: r'PUT',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );
=======
    ApiResponse? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null ? null : _serializers.deserialize(
        rawResponse,
        specifiedType: const FullType(ApiResponse),
      ) as ApiResponse;

    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart

    return await _dio.request<Object>(
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

  /// variant1Get
  ///
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
  /// Returns a [Future] containing a [Response] with a [FruitVariant1] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> variant1Get({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
=======
  /// Returns a [Future] containing a [Response] with a [ApiResponse] as data
  /// Throws [DioException] if API call or serialization fails
  Future<Response<ApiResponse>> uploadFileWithRequiredFile({ 
    required int petId,
    required MultipartFile requiredFile,
    String? additionalMetadata,
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
    final _path = r'/variant1';
=======
    final _path = r'/fake/{petId}/uploadImageWithRequiredFile'.replaceAll('{' r'petId' '}', encodeQueryParameter(_serializers, petId, const FullType(int)).toString());
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
    return await _dio.request<Object>(
=======
    dynamic _bodyData;

    try {
      _bodyData = FormData.fromMap(<String, dynamic>{
        if (additionalMetadata != null) r'additionalMetadata': encodeFormParameter(_serializers, additionalMetadata, const FullType(String)),
        r'requiredFile': requiredFile,
      });

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
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart
      _path,
      data: body,
      options: _options,
      cancelToken: cancelToken,
      onSendProgress: onSendProgress,
      onReceiveProgress: onReceiveProgress,
    );
  }

<<<<<<< HEAD:samples/openapi3/client/petstore/dart-dio/dio/built_value/lib/src/api/default_api.dart
  /// variant2Get
  ///
  ///
  /// Parameters:
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [extras] - Can be used to add flags to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  /// * [onSendProgress] - A [ProgressCallback] that can be used to get the send progress
  /// * [onReceiveProgress] - A [ProgressCallback] that can be used to get the receive progress
  ///
  /// Returns a [Future] containing a [Response] with a [FruitAllOfDisc] as data
  /// Throws [DioError] if API call or serialization fails
  Future<Response<Object>> variant2Get({
    Object? body,
    String? requestContentType,
    String? acceptContentType,
    ResponseType? responseType,
    CancelToken? cancelToken,
    Map<String, dynamic>? headers,
    Map<String, dynamic>? extra,
    ValidateStatus? validateStatus,
    ProgressCallback? onSendProgress,
    ProgressCallback? onReceiveProgress,
  }) async {
    final _path = r'/variant2';
    final _options = Options(
      method: r'GET',
      headers: <String, dynamic>{
        if (acceptContentType != null) 'Accept': acceptContentType,
        ...?headers,
      },
      extra: <String, dynamic>{
        'secure': <Map<String, String>>[],
        ...?extra,
      },
      contentType: requestContentType,
      responseType: responseType,
      validateStatus: validateStatus,
    );
=======
    ApiResponse? _responseData;

    try {
      final rawResponse = _response.data;
      _responseData = rawResponse == null ? null : _serializers.deserialize(
        rawResponse,
        specifiedType: const FullType(ApiResponse),
      ) as ApiResponse;

    } catch (error, stackTrace) {
      throw DioException(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioExceptionType.unknown,
        error: error,
        stackTrace: stackTrace,
      );
    }
>>>>>>> 95cefaeecdae21a43a453f07ed510c420abaa461:samples/openapi3/client/petstore/dart-dio/petstore_client_lib_fake/lib/src/api/pet_api.dart

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

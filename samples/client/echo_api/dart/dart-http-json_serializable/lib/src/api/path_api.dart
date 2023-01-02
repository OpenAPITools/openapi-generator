//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

//ignore: unused_import
import 'package:http/http.dart';
import 'dart:convert';
import 'package:openapi/src/deserialize.dart';

class PathApi {
  final Client _client;
  final String _basePath;

  const PathApi(this._client, this._basePath);

  /// Test path parameter(s)
  /// Test path parameter(s)
  ///
  /// Parameters:
  /// * [pathString]
  /// * [pathInteger]
  /// * [cancelToken] - A [CancelToken] that can be used to cancel the operation
  /// * [headers] - Can be used to add additional headers to the request
  /// * [validateStatus] - A [ValidateStatus] callback that can be used to determine request success based on the HTTP status of the response
  ///
  /// Throws [DioError] if API call or serialization fails

  Future<String> testsPathStringPathStringIntegerPathInteger({
    required String pathString,
    required int pathInteger,
    Map<String, String>? headers,
  }) async {
    final _path = r'/path/string/{path_string}/integer/{path_integer}'
        .replaceAll('{' r'path_string' '}', pathString.toString())
        .replaceAll('{' r'path_integer' '}', pathInteger.toString());
    final _uri = Uri.parse(_basePath + _path);

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

    String _responseData;

    try {
      _responseData = deserialize<String, String>(_response.data!, 'String',
          growable: true);
    } catch (error, stackTrace) {
      throw DioError(
        requestOptions: _response.requestOptions,
        response: _response,
        type: DioErrorType.other,
        error: error,
      )..stackTrace = stackTrace;
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
  }
}

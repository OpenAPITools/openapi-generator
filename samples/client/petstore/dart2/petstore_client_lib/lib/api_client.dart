//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ApiClient {
  ApiClient({this.basePath = 'http://petstore.swagger.io/v2'}) {
    // Setup authentications (key: authentication name, value: authentication).
    _authentications['api_key'] = ApiKeyAuth('header', 'api_key');
    _authentications['petstore_auth'] = OAuth();
  }

  final String basePath;
  final _defaultHeaderMap = <String, String>{};
  final _authentications = <String, Authentication>{};

  var _client = Client();

  /// Returns the current HTTP [Client] instance to use in this class.
  ///
  /// The return value is guaranteed to never be null.
  Client get client => _client;

  /// Requests to use a new HTTP [Client] in this class.
  ///
  /// If the [newClient] is null, an [ArgumentError] is thrown.
  set client(Client newClient) {
    if (newClient == null) {
      throw ArgumentError('New client instance cannot be null.');
    }
    _client = newClient;
  }

  /// Returns a copy of the [default headers][Map] that are sent with every request.
  Map<String, String> get defaultHeaderMap => <String, String>{}..addAll(_defaultHeaderMap);

  /// Adds, or updates if present, a header to the [default headers][Map] sent with every request.
  void addDefaultHeader(String key, String value) {
     _defaultHeaderMap[key] = value;
  }

  /// Removes a header, if present, from the [default headers][Map] sent with every request.
  void removeDefaultHeader(String key) {
     _defaultHeaderMap.remove(key);
  }

  dynamic deserialize(String json, String targetType, {bool growable}) {
    // Remove all spaces.  Necessary for reg expressions as well.
    targetType = targetType.replaceAll(' ', '');

    return targetType == 'String'
      ? json
      : _deserialize(jsonDecode(json), targetType, growable: true == growable);
  }

  String serialize(Object obj) => obj == null ? '' : json.encode(obj);

  T getAuthentication<T extends Authentication>(String name) {
    final authentication = _authentications[name];
    return authentication is T ? authentication : null;
  }

  /// Waits for a [StreamedResponse] to fully realize and returns the processed result as an
  /// HTTP [Response].
  Future<Response> getResponse(Future<StreamedResponse> streamedResponse) async =>
    Response.fromStream(await streamedResponse);

  /// Invokes an API endpoint and returns a [StreamedResponse] that can be listened to.
  ///
  /// This is useful if you want to upload a multipart or binary file and be able to listen
  /// to the upload progress.
  ///
  /// In the event of an HTTP error, this method will throw an [ApiException] and its
  /// [inner Exception][ApiException.innerException] would point to the erroneous cause.
  Future<StreamedResponse> streamAPI(
    String path,
    String method,
    Iterable<QueryParam> queryParams,
    Object body,
    Map<String, String> headerParams,
    Map<String, String> formParams,
    String nullableContentType,
    List<String> authNames,
  ) async {
    final request = buildRequest(
      path,
      method,
      queryParams,
      body,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );

    try {
      return _client.send(request);
    } on SocketException catch (err, trace) {
      throw ApiException.withInner(HttpStatus.badRequest, 'Socket operation failed: $method $path', err, trace);
    } on TlsException catch (err, trace) {
      throw ApiException.withInner(HttpStatus.badRequest, 'TLS/SSL communication failed: $method $path', err, trace);
    } on IOException catch (err, trace) {
      throw ApiException.withInner(HttpStatus.badRequest, 'I/O operation failed: $method $path', err, trace);
    } on Exception catch (err, trace) {
      throw ApiException.withInner(HttpStatus.badRequest, 'Exception occurred: $method $path', err, trace);
    }
  }

  /// Invokes an API endpoint and returns a fully realized HTTP [Response].
  ///
  /// In the event of an HTTP error, this method will throw an [ApiException] and its
  /// [inner Exception][ApiException.innerException] would point to the erroneous cause.
  Future<Response> invokeAPI(
    String path,
    String method,
    Iterable<QueryParam> queryParams,
    Object body,
    Map<String, String> headerParams,
    Map<String, String> formParams,
    String nullableContentType,
    List<String> authNames,
  ) => getResponse(streamAPI(
    path,
    method,
    queryParams,
    body,
    headerParams,
    formParams,
    nullableContentType,
    authNames,
  ));

  dynamic _deserialize(dynamic value, String targetType, {bool growable}) {
    try {
      switch (targetType) {
        case 'String':
          return '$value';
        case 'int':
          return value is int ? value : int.parse('$value');
        case 'bool':
          if (value is bool) {
            return value;
          }
          final valueString = '$value'.toLowerCase();
          return valueString == 'true' || valueString == '1';
          break;
        case 'double':
          return value is double ? value : double.parse('$value');
        case 'ApiResponse':
          return ApiResponse.fromJson(value);
        case 'Category':
          return Category.fromJson(value);
        case 'Order':
          return Order.fromJson(value);
        case 'Pet':
          return Pet.fromJson(value);
        case 'Tag':
          return Tag.fromJson(value);
        case 'User':
          return User.fromJson(value);
        default:
          Match match;
          if (value is List && (match = _regList.firstMatch(targetType)) != null) {
            final newTargetType = match[1];
            return value
              .map((v) => _deserialize(v, newTargetType, growable: growable))
              .toList(growable: true == growable);
          }
          if (value is Map && (match = _regMap.firstMatch(targetType)) != null) {
            final newTargetType = match[1];
            return Map.fromIterables(
              value.keys,
              value.values.map((v) => _deserialize(v, newTargetType, growable: growable)),
            );
          }
          break;
      }
    } on Exception catch (err, stack) {
      throw ApiException.withInner(HttpStatus.internalServerError, 'Exception during deserialization.', err, stack);
    }
    throw ApiException(HttpStatus.internalServerError, 'Could not find a suitable class for deserialization');
  }

  @protected
  BaseRequest buildRequest(
    String path,
    String method,
    Iterable<QueryParam> queryParams,
    Object body,
    Map<String, String> headerParams,
    Map<String, String> formParams,
    String nullableContentType,
    List<String> authNames,
  ) {
    final url = prepareUrlAndHeaderParams(
      path,
      queryParams,
      headerParams,
      nullableContentType,
      authNames,
    );
    final uri = Uri.parse(url);

    if (body is MultipartFile) {
      final request = StreamedRequest(method, uri);
      request.headers.addAll(headerParams);
      return request;
    }

    if (body is MultipartRequest) {
      final request = MultipartRequest(method, uri);
      request.fields.addAll(body.fields);
      request.files.addAll(body.files);
      request.headers.addAll(body.headers);
      request.headers.addAll(headerParams);
      return request;
    }

    final request = Request(method, uri);
    if (headerParams.isNotEmpty) {
      request.headers.addAll(headerParams);
    }
    final msgBody = nullableContentType == 'application/x-www-form-urlencoded' ? formParams : serialize(body);
    if (msgBody != null) {
      request.body = msgBody;
    }
    return request;
  }

  @protected
  String prepareUrlAndHeaderParams(
    String path,
    Iterable<QueryParam> queryParams,
    Map<String, String> headerParams,
    String nullableContentType,
    List<String> authNames,
  ) {
    updateParamsForAuth(authNames, queryParams, headerParams);
    headerParams.addAll(_defaultHeaderMap);
    if (nullableContentType != null) {
      headerParams['Content-Type'] = nullableContentType;
    }
    final queryString = queryParamsToString(queryParams);
    return '$basePath$path$queryString';
  }

  @protected
  void updateParamsForAuth(
    List<String> authNames,
    List<QueryParam> queryParams,
    Map<String, String> headerParams,
  ) {
    authNames.forEach((authName) {
      final auth = _authentications[authName];
      if (auth == null) {
        throw ArgumentError('Authentication undefined: $authName');
      }
      auth.applyToParams(queryParams, headerParams);
    });
  }
}

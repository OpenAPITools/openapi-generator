//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: avoid_init_to_null, lines_longer_than_80_chars
// ignore_for_file: prefer_single_quotes

part of openapi.api;

class QueryParam {
  QueryParam(this.name, this.value);

  String name;
  String value;
}

class ApiClient {
  ApiClient({this.basePath = "http://petstore.swagger.io/v2"}) {
    // Setup authentications (key: authentication name, value: authentication).
    _authentications["api_key"] = ApiKeyAuth("header", "api_key");
    _authentications["petstore_auth"] = OAuth();
  }

  String basePath;
  var client = Client();

  final _defaultHeaderMap = <String, String>{};
  final _authentications = <String, Authentication>{};

  void addDefaultHeader(String key, String value) {
     _defaultHeaderMap[key] = value;
  }

  dynamic deserialize(String json, String targetType, {bool growable}) {
    // Remove all spaces.  Necessary for reg expressions as well.
    targetType = targetType.replaceAll(" ", "");

    return targetType == "String"
      ? json
      : _deserialize(jsonDecode(json), targetType, growable: true == growable);
  }

  String serialize(Object obj) => obj == null ? "" : json.encode(obj);

  T getAuthentication<T extends Authentication>(String name) {
    final authentication = _authentications[name];
    return authentication is T ? authentication : null;
  }

  // We don’t use a Map<String, String> for queryParams.
  // If collectionFormat is "multi" a key might appear multiple times.
  Future<Response> invokeAPI(
    String path,
    String method,
    Iterable<QueryParam> queryParams,
    Object body,
    Map<String, String> headerParams,
    Map<String, String> formParams,
    String nullableContentType,
    List<String> authNames,
  ) async {
    _updateParamsForAuth(authNames, queryParams, headerParams);

    headerParams.addAll(_defaultHeaderMap);

    final ps = queryParams
      .where((p) => p.value != null)
      .map((p) => "${p.name}=${Uri.encodeQueryComponent(p.value)}");

    final queryString = ps.isNotEmpty ? "?" + ps.join("&") : "";

    final url = "$basePath$path$queryString";

    if (nullableContentType != null) {
      headerParams["Content-Type"] = nullableContentType;
    }

    if (body is MultipartRequest) {
      var request = MultipartRequest(method, Uri.parse(url));
      request.fields.addAll(body.fields);
      request.files.addAll(body.files);
      request.headers.addAll(body.headers);
      request.headers.addAll(headerParams);
      var response = await client.send(request);
      return Response.fromStream(response);
    }

    final msgBody = nullableContentType == "application/x-www-form-urlencoded"
      ? formParams
      : serialize(body);
    final nullableHeaderParams = headerParams.isEmpty ? null : headerParams;

    try {
      switch(method) {
        case "POST": return await client.post(url, headers: nullableHeaderParams, body: msgBody);
        case "PUT": return await client.put(url, headers: nullableHeaderParams, body: msgBody);
        case "DELETE": return await client.delete(url, headers: nullableHeaderParams);
        case "PATCH": return await client.patch(url, headers: nullableHeaderParams, body: msgBody);
        case "HEAD": return await client.head(url, headers: nullableHeaderParams);
        case "GET": return await client.get(url, headers: nullableHeaderParams);
      }
    } on SocketException catch (e, trace) {
      throw ApiException.withInner(400, "Socket operation failed: $method $path", e, trace);
    } on TlsException catch (e, trace) {
      throw ApiException.withInner(400, "TLS/SSL communication failed: $method $path", e, trace);
    } on IOException catch (e, trace) {
      throw ApiException.withInner(400, "I/O operation failed: $method $path", e, trace);
    } on Exception catch (e, trace) {
      throw ApiException.withInner(400, "Exception occurred: $method $path", e, trace);
    }

    throw ApiException(400, "Invalid HTTP operation: $method $path");
  }

  dynamic _deserialize(dynamic value, String targetType, {bool growable}) {
    try {
      switch (targetType) {
        case "String":
          return "$value";
        case "int":
          return value is int ? value : int.parse("$value");
        case "bool":
          if (value is bool) return value;
          final valueString = "$value".toLowerCase();
          return valueString == "true" || valueString == "1";
          break;
        case "double":
          return value is double ? value : double.parse("$value");
        case "ApiResponse":
          return ApiResponse.fromJson(value);
        case "Category":
          return Category.fromJson(value);
        case "Order":
          return Order.fromJson(value);
        case "Pet":
          return Pet.fromJson(value);
        case "Tag":
          return Tag.fromJson(value);
        case "User":
          return User.fromJson(value);
        default:
          Match match;
          if (value is List && (match = _regList.firstMatch(targetType)) != null) {
            var newTargetType = match[1];
            return value
              .map((v) => _deserialize(v, newTargetType, growable: growable))
              .toList(growable: true == growable);
          }
          if (value is Map && (match = _regMap.firstMatch(targetType)) != null) {
            var newTargetType = match[1];
            return Map.fromIterables(
              value.keys,
              value.values.map((v) => _deserialize(v, newTargetType, growable: growable)),
            );
          }
          break;
      }
    } on Exception catch (e, stack) {
      throw ApiException.withInner(500, "Exception during deserialization.", e, stack);
    }
    throw ApiException(500, "Could not find a suitable class for deserialization");
  }

  /// Update query and header parameters based on authentication settings.
  /// @param authNames The authentications to apply
  void _updateParamsForAuth(
    List<String> authNames,
    List<QueryParam> queryParams,
    Map<String, String> headerParams,
  ) {
    authNames.forEach((authName) {
      final auth = _authentications[authName];
      if (auth == null) {
        throw ArgumentError("Authentication undefined: $authName");
      }
      auth.applyToParams(queryParams, headerParams);
    });
  }
}

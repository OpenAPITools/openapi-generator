//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:http/http.dart' as http;
import 'package:built_value/serializer.dart';
import 'package:openapi/src/serializers.dart';
import 'package:openapi/src/auth/_exports.dart';
import 'package:openapi/src/api/body_api.dart';
import 'package:openapi/src/api/path_api.dart';
import 'package:openapi/src/api/query_api.dart';

class Openapi {
  static const String basePath = r'http://localhost:3000';

  final http.Client client;
  final String actualBasePath;
  final Serializers serializers;

  Openapi({
    http.Client? client,
    Serializers? serializers,
    String? basePathOverride,
  })  : this.serializers = serializers ?? standardSerializers,
        this.client = client ?? http.Client(),
        this.actualBasePath = basePathOverride ?? basePath;

  /// Get BodyApi instance, base route and serializer can be overridden by a given but be careful,
  /// by doing that all interceptors will not be executed
  BodyApi getBodyApi() {
    return BodyApi(client, actualBasePath, serializers);
  }

  /// Get PathApi instance, base route and serializer can be overridden by a given but be careful,
  /// by doing that all interceptors will not be executed
  PathApi getPathApi() {
    return PathApi(client, actualBasePath, serializers);
  }

  /// Get QueryApi instance, base route and serializer can be overridden by a given but be careful,
  /// by doing that all interceptors will not be executed
  QueryApi getQueryApi() {
    return QueryApi(client, actualBasePath, serializers);
  }
}

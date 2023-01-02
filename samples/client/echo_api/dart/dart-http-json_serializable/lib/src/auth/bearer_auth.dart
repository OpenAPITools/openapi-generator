//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:openapi/src/api_util.dart';
import 'dart:convert';

import 'auth.dart';

typedef HttpBearerAuthProvider = String? Function();

class HttpBearerAuth implements Authentication {
  HttpBearerAuth();

  HttpBearerAuthProvider? accessTokenProvider;

  @override
  Future<void> applyToParams(
    List<QueryParam> queryParams,
    Map<String, String> headerParams,
  ) async {
    final provider = accessTokenProvider;
    if (provider == null) {
      return;
    }

    final accessToken = provider();
    if (accessToken != null && accessToken.isNotEmpty) {
      headerParams['Authorization'] = 'Bearer $accessToken';
    }
  }
}

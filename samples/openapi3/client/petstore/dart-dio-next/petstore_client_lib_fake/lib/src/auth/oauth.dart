//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'dart:async';

import 'package:dio/dio.dart';
import 'package:openapi/src/auth/auth.dart';

class OAuthInterceptor extends AuthInterceptor {
    Map<String, String> tokens = {};

    @override
    Future<dynamic> onRequest(RequestOptions options) {
        final authInfo = getAuthInfo(options, 'oauth');
        for (final info in authInfo) {
            final token = tokens[info['name']];
            if (token != null) {
                options.headers['Authorization'] = 'Bearer ${token}';
                break;
            }
        }
        return super.onRequest(options);
    }
}

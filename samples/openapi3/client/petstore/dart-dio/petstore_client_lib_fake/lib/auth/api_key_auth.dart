import 'dart:async';
import 'package:openapi/auth/auth.dart';
import 'package:dio/dio.dart';

class ApiKeyAuthInterceptor extends AuthInterceptor {
    Map<String, String> apiKeys = {};

    @override
    Future<dynamic> onRequest(RequestOptions options) {
        final authInfo = getAuthInfo(options, 'apiKey');
        for (final info in authInfo) {
            final String authName = info['name'];
            final String authKeyName = info['keyName'];
            final String authWhere = info['where'];
            final apiKey = apiKeys[authName];
            if (apiKey != null) {
                if (authWhere == 'query') {
                    options.queryParameters[authKeyName] = apiKey;
                } else {
                    options.headers[authKeyName] = apiKey;
                }
                break;
            }
        }
        return super.onRequest(options);
    }
}

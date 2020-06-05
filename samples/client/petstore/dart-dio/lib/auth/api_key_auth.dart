import 'dart:async';
import 'package:openapi/auth/auth.dart';
import 'package:dio/dio.dart';

class ApiKeyAuthInterceptor extends AuthInterceptor {
    Map<String, String> apiKeys = {};

    @override
    Future onRequest(RequestOptions options) {
        final authInfo = getAuthInfo(options, "apiKey");
        for (var info in authInfo) {
            final authName = info["name"];
            final authKeyName = info["keyName"];
            final authWhere = info["where"];
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

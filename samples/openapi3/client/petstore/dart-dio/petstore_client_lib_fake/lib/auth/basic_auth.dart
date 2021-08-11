//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'dart:async';
import 'dart:convert';
import 'package:openapi/auth/auth.dart';
import 'package:dio/dio.dart';

class BasicAuthInfo {
    final String username;
    final String password;

    const BasicAuthInfo(this.username, this.password);
}

class BasicAuthInterceptor extends AuthInterceptor {
    Map<String, BasicAuthInfo> authInfo = {};

    @override
    Future<dynamic> onRequest(RequestOptions options) {
        final metadataAuthInfo = getAuthInfo(options, 'basic');
        for (final info in metadataAuthInfo) {
            final authName = info['name'] as String;
            final basicAuthInfo = authInfo[authName];
            if (basicAuthInfo != null) {
                final basicAuth = 'Basic ' + base64Encode(utf8.encode('${basicAuthInfo.username}:${basicAuthInfo.password}'));
                options.headers['Authorization'] = basicAuth;
                break;
            }
        }

        return super.onRequest(options);
    }
}

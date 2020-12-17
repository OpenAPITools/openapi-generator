import 'dart:async';

import 'package:dio/dio.dart';

abstract class AuthInterceptor extends Interceptor {
    /*
     * Get auth information on given route for the given type
     * Can return null if type is not present on auth data or if route doesn't need authentication
     */
    List<Map<String, dynamic>> getAuthInfo(RequestOptions route, String type) {
        if (route.extra.containsKey("secure")) {
            final auth = route.extra["secure"];
            List<Map<String, dynamic>> results = [];
            for (var info in auth) {
                if(info["type"] == type) {
                    results.add(info);
                }
            }
            return results;
        }
        return [];
    }
}

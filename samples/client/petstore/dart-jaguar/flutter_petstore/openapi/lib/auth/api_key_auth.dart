import 'dart:async';
import 'package:openapi/auth/auth.dart';
import 'package:jaguar_retrofit/jaguar_retrofit.dart';

class ApiKeyAuthInterceptor extends AuthInterceptor {
    Map<String, String> apiKeys = {};

    @override
    FutureOr<void> before(RouteBase route) {
        final authInfo = getAuthInfo(route, "apiKey");
        for (var info in authInfo) {
            final authName = info["name"];
            final authKeyName = info["keyName"];
            final authWhere = info["where"];
            final apiKey = apiKeys[authName];
            if(apiKey != null) {
                if(authWhere == 'query'){
                    route.query(authKeyName, apiKey);
                }
            else {
                    route.header(authKeyName, apiKey);
                }
                break;
            }
        }
        return super.before(route);
    }

    @override
    FutureOr after(StringResponse response) {
        return Future.value(response);
    }
}
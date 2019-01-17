import 'dart:async';
import 'package:openapi/auth/auth.dart';
import 'package:jaguar_retrofit/jaguar_retrofit.dart';

class BasicAuthInfo {
    final String username;
    final String password;

    const BasicAuthInfo(this.username, this.password);

}

class BasicAuthInterceptor extends AuthInterceptor {
    Map<String, BasicAuthInfo> authInfo = {};

    @override
    FutureOr<void> before(RouteBase route) {
        final metadataAuthInfo = getAuthInfo(route, "basic");
        for (var info in metadataAuthInfo) {
            final authName = info["name"];
            final basicAuthInfo = authInfo[authName];
            if(basicAuthInfo != null) {
                route.basicAuth(basicAuthInfo.username, basicAuthInfo.password);
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
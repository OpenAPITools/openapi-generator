// ignore_for_file: unnecessary_type_check

part of 'user_api.dart';


class UserApiCreateUserResponse {
  UserApiCreateUserResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.context,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> context;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(UserApiCreateUserResponse0 response) on0,
    required T Function(UserApiCreateUserResponse response) other,
  }) {
    return switch (this) {
      UserApiCreateUserResponse0 response => on0(response),
      _ => other(this),
    };
  }

  static Future<UserApiCreateUserResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiCreateUserResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'0'),
      () => UserApiCreateUserResponse0.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiCreateUserResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class UserApiCreateUserResponse0 extends UserApiCreateUserResponse {
  UserApiCreateUserResponse0({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<UserApiCreateUserResponse0> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return UserApiCreateUserResponse0(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiCreateUsersWithArrayInputResponse {
  UserApiCreateUsersWithArrayInputResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.context,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> context;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(UserApiCreateUsersWithArrayInputResponse0 response) on0,
    required T Function(UserApiCreateUsersWithArrayInputResponse response) other,
  }) {
    return switch (this) {
      UserApiCreateUsersWithArrayInputResponse0 response => on0(response),
      _ => other(this),
    };
  }

  static Future<UserApiCreateUsersWithArrayInputResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiCreateUsersWithArrayInputResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'0'),
      () => UserApiCreateUsersWithArrayInputResponse0.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiCreateUsersWithArrayInputResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class UserApiCreateUsersWithArrayInputResponse0 extends UserApiCreateUsersWithArrayInputResponse {
  UserApiCreateUsersWithArrayInputResponse0({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<UserApiCreateUsersWithArrayInputResponse0> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return UserApiCreateUsersWithArrayInputResponse0(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiCreateUsersWithListInputResponse {
  UserApiCreateUsersWithListInputResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.context,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> context;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(UserApiCreateUsersWithListInputResponse0 response) on0,
    required T Function(UserApiCreateUsersWithListInputResponse response) other,
  }) {
    return switch (this) {
      UserApiCreateUsersWithListInputResponse0 response => on0(response),
      _ => other(this),
    };
  }

  static Future<UserApiCreateUsersWithListInputResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiCreateUsersWithListInputResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'0'),
      () => UserApiCreateUsersWithListInputResponse0.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiCreateUsersWithListInputResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class UserApiCreateUsersWithListInputResponse0 extends UserApiCreateUsersWithListInputResponse {
  UserApiCreateUsersWithListInputResponse0({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<UserApiCreateUsersWithListInputResponse0> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return UserApiCreateUsersWithListInputResponse0(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiDeleteUserResponse {
  UserApiDeleteUserResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.context,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> context;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(UserApiDeleteUserResponse400 response) on400,
    required T Function(UserApiDeleteUserResponse404 response) on404,
    required T Function(UserApiDeleteUserResponse response) other,
  }) {
    return switch (this) {
      UserApiDeleteUserResponse400 response => on400(response),
      UserApiDeleteUserResponse404 response => on404(response),
      _ => other(this),
    };
  }

  static Future<UserApiDeleteUserResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiDeleteUserResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => UserApiDeleteUserResponse400.fromResponse(response, context: context)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'404'),
      () => UserApiDeleteUserResponse404.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiDeleteUserResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class UserApiDeleteUserResponse400 extends UserApiDeleteUserResponse {
  UserApiDeleteUserResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<UserApiDeleteUserResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return UserApiDeleteUserResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class UserApiDeleteUserResponse404 extends UserApiDeleteUserResponse {
  UserApiDeleteUserResponse404({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<UserApiDeleteUserResponse404> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return UserApiDeleteUserResponse404(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiGetUserByNameResponse {
  UserApiGetUserByNameResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.context,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> context;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(UserApiGetUserByNameResponse200 response) on200,
    required T Function(UserApiGetUserByNameResponse400 response) on400,
    required T Function(UserApiGetUserByNameResponse404 response) on404,
    required T Function(UserApiGetUserByNameResponse response) other,
  }) {
    return switch (this) {
      UserApiGetUserByNameResponse200 response => on200(response),
      UserApiGetUserByNameResponse400 response => on400(response),
      UserApiGetUserByNameResponse404 response => on404(response),
      _ => other(this),
    };
  }

  static Future<UserApiGetUserByNameResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiGetUserByNameResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => UserApiGetUserByNameResponse200.fromResponse(response, context: context)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => UserApiGetUserByNameResponse400.fromResponse(response, context: context)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'404'),
      () => UserApiGetUserByNameResponse404.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiGetUserByNameResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class UserApiGetUserByNameResponse200 extends UserApiGetUserByNameResponse {
  UserApiGetUserByNameResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });

  T split200<T>({
    
    required T Function(UserApiGetUserByNameResponse200ApplicationXml response) onApplicationXml,
    
    required T Function(UserApiGetUserByNameResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(UserApiGetUserByNameResponse200 response) other,
  }) {
    return switch (this) {
      
      UserApiGetUserByNameResponse200ApplicationXml response => onApplicationXml(response),
      
      UserApiGetUserByNameResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<UserApiGetUserByNameResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<UserApiGetUserByNameResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/xml')),
        () => UserApiGetUserByNameResponse200ApplicationXml.fromResponse(response, contentType: contentTypeParsed, context: context)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => UserApiGetUserByNameResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, context: context)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return UserApiGetUserByNameResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



/// Represent the response when content-type is application/xml.
class UserApiGetUserByNameResponse200ApplicationXml extends UserApiGetUserByNameResponse200 {
  final UndefinedWrapper<
            User
> body;

  /// The raw result of calling XmlDocument.parse
  final XmlDocument? rawXml;

  UserApiGetUserByNameResponse200ApplicationXml({
     this.body = const UndefinedWrapper
        .undefined()
,
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
    this.rawXml,
  });

  static Future<UserApiGetUserByNameResponse200ApplicationXml> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> context}) async {
    final charset = contentType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    switch (contentType) {
      
      
      case MediaType(type: 'application', subtype: 'xml'):
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = XmlDocument.parse(serialized);
        // check if v can be deserialized to xml
        return UserApiGetUserByNameResponse200ApplicationXml(
          headers: response.headers,
          statusCode: response.statusCode,
          reasonPhrase: response.reasonPhrase,
          context: context,
          rawXml: v,
        );
      
    }
    return UserApiGetUserByNameResponse200ApplicationXml(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

/// Represent the response when content-type is application/json.
class UserApiGetUserByNameResponse200ApplicationJson extends UserApiGetUserByNameResponse200 {
  final UndefinedWrapper<
            User
> body;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  UserApiGetUserByNameResponse200ApplicationJson({
     this.body = const UndefinedWrapper
        .undefined()
,
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<UserApiGetUserByNameResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> context}) async {
    final charset = contentType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    switch (contentType) {
      
      case MediaType(type: 'application', subtype: 'json'):
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (v == null ? false :
(

    
            User.canDeserialize(v)
            
)) {
          final res = User.deserialize
(

    
            v


)


;
          return UserApiGetUserByNameResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            body: UndefinedWrapper(res),
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return UserApiGetUserByNameResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            rawJson: v,
          );
        }
      
      
    }
    return UserApiGetUserByNameResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class UserApiGetUserByNameResponse400 extends UserApiGetUserByNameResponse {
  UserApiGetUserByNameResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<UserApiGetUserByNameResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return UserApiGetUserByNameResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class UserApiGetUserByNameResponse404 extends UserApiGetUserByNameResponse {
  UserApiGetUserByNameResponse404({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<UserApiGetUserByNameResponse404> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return UserApiGetUserByNameResponse404(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiLoginUserResponse {
  UserApiLoginUserResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.context,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> context;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(UserApiLoginUserResponse200 response) on200,
    required T Function(UserApiLoginUserResponse400 response) on400,
    required T Function(UserApiLoginUserResponse response) other,
  }) {
    return switch (this) {
      UserApiLoginUserResponse200 response => on200(response),
      UserApiLoginUserResponse400 response => on400(response),
      _ => other(this),
    };
  }

  static Future<UserApiLoginUserResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiLoginUserResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => UserApiLoginUserResponse200.fromResponse(response, context: context)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => UserApiLoginUserResponse400.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiLoginUserResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class UserApiLoginUserResponse200 extends UserApiLoginUserResponse {
  UserApiLoginUserResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });

  T split200<T>({
    
    required T Function(UserApiLoginUserResponse200ApplicationXml response) onApplicationXml,
    
    required T Function(UserApiLoginUserResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(UserApiLoginUserResponse200 response) other,
  }) {
    return switch (this) {
      
      UserApiLoginUserResponse200ApplicationXml response => onApplicationXml(response),
      
      UserApiLoginUserResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<UserApiLoginUserResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<UserApiLoginUserResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/xml')),
        () => UserApiLoginUserResponse200ApplicationXml.fromResponse(response, contentType: contentTypeParsed, context: context)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => UserApiLoginUserResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, context: context)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return UserApiLoginUserResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



/// Represent the response when content-type is application/xml.
class UserApiLoginUserResponse200ApplicationXml extends UserApiLoginUserResponse200 {
  final UndefinedWrapper<
            String
> body;

  /// The raw result of calling XmlDocument.parse
  final XmlDocument? rawXml;

  UserApiLoginUserResponse200ApplicationXml({
     this.body = const UndefinedWrapper
        .undefined()
,
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
    this.rawXml,
  });

  static Future<UserApiLoginUserResponse200ApplicationXml> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> context}) async {
    final charset = contentType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    switch (contentType) {
      
      
      case MediaType(type: 'application', subtype: 'xml'):
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = XmlDocument.parse(serialized);
        // check if v can be deserialized to xml
        return UserApiLoginUserResponse200ApplicationXml(
          headers: response.headers,
          statusCode: response.statusCode,
          reasonPhrase: response.reasonPhrase,
          context: context,
          rawXml: v,
        );
      
    }
    return UserApiLoginUserResponse200ApplicationXml(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

/// Represent the response when content-type is application/json.
class UserApiLoginUserResponse200ApplicationJson extends UserApiLoginUserResponse200 {
  final UndefinedWrapper<
            String
> body;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  UserApiLoginUserResponse200ApplicationJson({
     this.body = const UndefinedWrapper
        .undefined()
,
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<UserApiLoginUserResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> context}) async {
    final charset = contentType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    switch (contentType) {
      
      case MediaType(type: 'application', subtype: 'json'):
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (v == null ? false :
(

    
            
            v is String
)) {
          final res = 
(

    
            
                    v as String
            

)


;
          return UserApiLoginUserResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            body: UndefinedWrapper(res),
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return UserApiLoginUserResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            rawJson: v,
          );
        }
      
      
    }
    return UserApiLoginUserResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class UserApiLoginUserResponse400 extends UserApiLoginUserResponse {
  UserApiLoginUserResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<UserApiLoginUserResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return UserApiLoginUserResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiLogoutUserResponse {
  UserApiLogoutUserResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.context,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> context;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(UserApiLogoutUserResponse0 response) on0,
    required T Function(UserApiLogoutUserResponse response) other,
  }) {
    return switch (this) {
      UserApiLogoutUserResponse0 response => on0(response),
      _ => other(this),
    };
  }

  static Future<UserApiLogoutUserResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiLogoutUserResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'0'),
      () => UserApiLogoutUserResponse0.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiLogoutUserResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class UserApiLogoutUserResponse0 extends UserApiLogoutUserResponse {
  UserApiLogoutUserResponse0({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<UserApiLogoutUserResponse0> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return UserApiLogoutUserResponse0(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiUpdateUserResponse {
  UserApiUpdateUserResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.context,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> context;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(UserApiUpdateUserResponse400 response) on400,
    required T Function(UserApiUpdateUserResponse404 response) on404,
    required T Function(UserApiUpdateUserResponse response) other,
  }) {
    return switch (this) {
      UserApiUpdateUserResponse400 response => on400(response),
      UserApiUpdateUserResponse404 response => on404(response),
      _ => other(this),
    };
  }

  static Future<UserApiUpdateUserResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiUpdateUserResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => UserApiUpdateUserResponse400.fromResponse(response, context: context)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'404'),
      () => UserApiUpdateUserResponse404.fromResponse(response, context: context)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiUpdateUserResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class UserApiUpdateUserResponse400 extends UserApiUpdateUserResponse {
  UserApiUpdateUserResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<UserApiUpdateUserResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return UserApiUpdateUserResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class UserApiUpdateUserResponse404 extends UserApiUpdateUserResponse {
  UserApiUpdateUserResponse404({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<UserApiUpdateUserResponse404> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return UserApiUpdateUserResponse404(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}







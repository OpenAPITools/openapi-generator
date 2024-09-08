// ignore_for_file: unnecessary_type_check, unnecessary_null_comparison, unnecessary_cast

part of 'user_api.dart';


class UserApiCreateUserResponse {
  UserApiCreateUserResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.userContext,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> userContext;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(UserApiCreateUserResponseDefault response) onDefault,
    required T Function(UserApiCreateUserResponse response) other,
  }) {
    return switch (this) {
      UserApiCreateUserResponseDefault response => onDefault(response),
      _ => other(this),
    };
  }

  static Future<UserApiCreateUserResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiCreateUserResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'default'),
      () => UserApiCreateUserResponseDefault.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiCreateUserResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class UserApiCreateUserResponseDefault extends UserApiCreateUserResponse {
  UserApiCreateUserResponseDefault({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<UserApiCreateUserResponseDefault> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return UserApiCreateUserResponseDefault(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiCreateUsersWithArrayInputResponse {
  UserApiCreateUsersWithArrayInputResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.userContext,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> userContext;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(UserApiCreateUsersWithArrayInputResponseDefault response) onDefault,
    required T Function(UserApiCreateUsersWithArrayInputResponse response) other,
  }) {
    return switch (this) {
      UserApiCreateUsersWithArrayInputResponseDefault response => onDefault(response),
      _ => other(this),
    };
  }

  static Future<UserApiCreateUsersWithArrayInputResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiCreateUsersWithArrayInputResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'default'),
      () => UserApiCreateUsersWithArrayInputResponseDefault.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiCreateUsersWithArrayInputResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class UserApiCreateUsersWithArrayInputResponseDefault extends UserApiCreateUsersWithArrayInputResponse {
  UserApiCreateUsersWithArrayInputResponseDefault({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<UserApiCreateUsersWithArrayInputResponseDefault> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return UserApiCreateUsersWithArrayInputResponseDefault(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiCreateUsersWithListInputResponse {
  UserApiCreateUsersWithListInputResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.userContext,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> userContext;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(UserApiCreateUsersWithListInputResponseDefault response) onDefault,
    required T Function(UserApiCreateUsersWithListInputResponse response) other,
  }) {
    return switch (this) {
      UserApiCreateUsersWithListInputResponseDefault response => onDefault(response),
      _ => other(this),
    };
  }

  static Future<UserApiCreateUsersWithListInputResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiCreateUsersWithListInputResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'default'),
      () => UserApiCreateUsersWithListInputResponseDefault.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiCreateUsersWithListInputResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class UserApiCreateUsersWithListInputResponseDefault extends UserApiCreateUsersWithListInputResponse {
  UserApiCreateUsersWithListInputResponseDefault({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<UserApiCreateUsersWithListInputResponseDefault> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return UserApiCreateUsersWithListInputResponseDefault(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiDeleteUserResponse {
  UserApiDeleteUserResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.userContext,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> userContext;
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

  static Future<UserApiDeleteUserResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiDeleteUserResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => UserApiDeleteUserResponse400.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'404'),
      () => UserApiDeleteUserResponse404.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiDeleteUserResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class UserApiDeleteUserResponse400 extends UserApiDeleteUserResponse {
  UserApiDeleteUserResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<UserApiDeleteUserResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return UserApiDeleteUserResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}





class UserApiDeleteUserResponse404 extends UserApiDeleteUserResponse {
  UserApiDeleteUserResponse404({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<UserApiDeleteUserResponse404> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return UserApiDeleteUserResponse404(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiGetUserByNameResponse {
  UserApiGetUserByNameResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.userContext,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> userContext;
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

  static Future<UserApiGetUserByNameResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiGetUserByNameResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => UserApiGetUserByNameResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => UserApiGetUserByNameResponse400.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'404'),
      () => UserApiGetUserByNameResponse404.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiGetUserByNameResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class UserApiGetUserByNameResponse200 extends UserApiGetUserByNameResponse {
  UserApiGetUserByNameResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
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

  static Future<UserApiGetUserByNameResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<UserApiGetUserByNameResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/xml')),
        () => UserApiGetUserByNameResponse200ApplicationXml.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => UserApiGetUserByNameResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
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
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/xml.
class UserApiGetUserByNameResponse200ApplicationXml extends UserApiGetUserByNameResponse200 {
  final UndefinedWrapper<
            User
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'User',
),
    
            
        
        
            
                User.$reflection
        
,
)
;

  /// The raw result of calling XmlDocumentFragment.parse
  final XmlDocumentFragment? rawXml;

  UserApiGetUserByNameResponse200ApplicationXml({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawXml,
  });

  static Future<UserApiGetUserByNameResponse200ApplicationXml> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'xml'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = XmlDocumentFragment.parse(serialized);
        // check if v can be deserialized to xml
        return UserApiGetUserByNameResponse200ApplicationXml(
          headers: response.headers,
          statusCode: response.statusCode,
          reasonPhrase: response.reasonPhrase,
          userContext: userContext,
          rawXml: v,
          
        );
      default:
    }
    return UserApiGetUserByNameResponse200ApplicationXml(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}






/// Represent the response when content-type is application/json.
class UserApiGetUserByNameResponse200ApplicationJson extends UserApiGetUserByNameResponse200 {
  final UndefinedWrapper<
            User
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'User',
),
    
            
        
        
            
                User.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  UserApiGetUserByNameResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<UserApiGetUserByNameResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'json'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (bodyReflection.canDeserializeFunction(v, context)) {
          final res = bodyReflection.deserializeFunction(v, context);
          return UserApiGetUserByNameResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return UserApiGetUserByNameResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return UserApiGetUserByNameResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}



class UserApiGetUserByNameResponse400 extends UserApiGetUserByNameResponse {
  UserApiGetUserByNameResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<UserApiGetUserByNameResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return UserApiGetUserByNameResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}





class UserApiGetUserByNameResponse404 extends UserApiGetUserByNameResponse {
  UserApiGetUserByNameResponse404({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<UserApiGetUserByNameResponse404> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return UserApiGetUserByNameResponse404(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiLoginUserResponse {
  UserApiLoginUserResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.userContext,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> userContext;
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

  static Future<UserApiLoginUserResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiLoginUserResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => UserApiLoginUserResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => UserApiLoginUserResponse400.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiLoginUserResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}











class UserApiLoginUserResponse200 extends UserApiLoginUserResponse {
  UserApiLoginUserResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
     this.xRateLimit= const UndefinedWrapper
        .undefined()
,
     this.xExpiresAfter= const UndefinedWrapper
        .undefined()
,
  });

  final UndefinedWrapper<
            int
> xRateLimit;
  final UndefinedWrapper<
            DateTime
> xExpiresAfter;

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

  static Future<UserApiLoginUserResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<UserApiLoginUserResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/xml')),
        () => UserApiLoginUserResponse200ApplicationXml.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => UserApiLoginUserResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
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
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/xml.
class UserApiLoginUserResponse200ApplicationXml extends UserApiLoginUserResponse200 {
  final UndefinedWrapper<
            String
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
;

  /// The raw result of calling XmlDocumentFragment.parse
  final XmlDocumentFragment? rawXml;

  UserApiLoginUserResponse200ApplicationXml({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
     super.xRateLimit,
     super.xExpiresAfter,
    this.rawXml,
  });

  static Future<UserApiLoginUserResponse200ApplicationXml> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'xml'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = XmlDocumentFragment.parse(serialized);
        // check if v can be deserialized to xml
        return UserApiLoginUserResponse200ApplicationXml(
          headers: response.headers,
          statusCode: response.statusCode,
          reasonPhrase: response.reasonPhrase,
          userContext: userContext,
          rawXml: v,
          xRateLimit: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Rate-Limit')),
xExpiresAfter: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Expires-After')),

        );
      default:
    }
    return UserApiLoginUserResponse200ApplicationXml(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      xRateLimit: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Rate-Limit')),
xExpiresAfter: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Expires-After')),

    );
  }
}






/// Represent the response when content-type is application/json.
class UserApiLoginUserResponse200ApplicationJson extends UserApiLoginUserResponse200 {
  final UndefinedWrapper<
            String
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  UserApiLoginUserResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
     super.xRateLimit,
     super.xExpiresAfter,
    this.rawJson,
  });

  static Future<UserApiLoginUserResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'json'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (bodyReflection.canDeserializeFunction(v, context)) {
          final res = bodyReflection.deserializeFunction(v, context);
          return UserApiLoginUserResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            xRateLimit: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Rate-Limit')),
xExpiresAfter: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Expires-After')),

          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return UserApiLoginUserResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            xRateLimit: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Rate-Limit')),
xExpiresAfter: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Expires-After')),

          );
        }
      default:
    }
    return UserApiLoginUserResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      xRateLimit: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Rate-Limit')),
xExpiresAfter: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
).deserializeFunction(response.headers.getOrUndefined(r'X-Expires-After')),

    );
  }
}



class UserApiLoginUserResponse400 extends UserApiLoginUserResponse {
  UserApiLoginUserResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<UserApiLoginUserResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return UserApiLoginUserResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiLogoutUserResponse {
  UserApiLogoutUserResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.userContext,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> userContext;
  /// This variable is only assigned if other response classes fail to read the response.
  /// Thus, handing the responsibility of reading the response to the user.
  final Stream<List<int>>? bodyBytesStream;

  T split<T>({
    required T Function(UserApiLogoutUserResponseDefault response) onDefault,
    required T Function(UserApiLogoutUserResponse response) other,
  }) {
    return switch (this) {
      UserApiLogoutUserResponseDefault response => onDefault(response),
      _ => other(this),
    };
  }

  static Future<UserApiLogoutUserResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiLogoutUserResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'default'),
      () => UserApiLogoutUserResponseDefault.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiLogoutUserResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class UserApiLogoutUserResponseDefault extends UserApiLogoutUserResponse {
  UserApiLogoutUserResponseDefault({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<UserApiLogoutUserResponseDefault> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return UserApiLogoutUserResponseDefault(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class UserApiUpdateUserResponse {
  UserApiUpdateUserResponse({
    required this.headers,
    required this.statusCode,
    required this.reasonPhrase,
    required this.userContext,
    this.bodyBytesStream,
  });

  final Map<String, String> headers;
  final int statusCode;
  final String? reasonPhrase;
  final Map<String, dynamic> userContext;
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

  static Future<UserApiUpdateUserResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<UserApiUpdateUserResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => UserApiUpdateUserResponse400.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'404'),
      () => UserApiUpdateUserResponse404.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return UserApiUpdateUserResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class UserApiUpdateUserResponse400 extends UserApiUpdateUserResponse {
  UserApiUpdateUserResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<UserApiUpdateUserResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return UserApiUpdateUserResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}





class UserApiUpdateUserResponse404 extends UserApiUpdateUserResponse {
  UserApiUpdateUserResponse404({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<UserApiUpdateUserResponse404> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return UserApiUpdateUserResponse404(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}







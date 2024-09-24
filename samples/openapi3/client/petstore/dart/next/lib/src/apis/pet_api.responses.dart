// ignore_for_file: unnecessary_type_check, unnecessary_null_comparison, unnecessary_cast

part of 'pet_api.dart';


class PetApiAddPetResponse {
  PetApiAddPetResponse({
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
    required T Function(PetApiAddPetResponse405 response) on405,
    required T Function(PetApiAddPetResponse response) other,
  }) {
    return switch (this) {
      PetApiAddPetResponse405 response => on405(response),
      _ => other(this),
    };
  }

  static Future<PetApiAddPetResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<PetApiAddPetResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'405'),
      () => PetApiAddPetResponse405.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return PetApiAddPetResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class PetApiAddPetResponse405 extends PetApiAddPetResponse {
  PetApiAddPetResponse405({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<PetApiAddPetResponse405> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return PetApiAddPetResponse405(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class PetApiDeletePetResponse {
  PetApiDeletePetResponse({
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
    required T Function(PetApiDeletePetResponse400 response) on400,
    required T Function(PetApiDeletePetResponse response) other,
  }) {
    return switch (this) {
      PetApiDeletePetResponse400 response => on400(response),
      _ => other(this),
    };
  }

  static Future<PetApiDeletePetResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<PetApiDeletePetResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => PetApiDeletePetResponse400.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return PetApiDeletePetResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class PetApiDeletePetResponse400 extends PetApiDeletePetResponse {
  PetApiDeletePetResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<PetApiDeletePetResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return PetApiDeletePetResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class PetApiFindPetsByStatusResponse {
  PetApiFindPetsByStatusResponse({
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
    required T Function(PetApiFindPetsByStatusResponse200 response) on200,
    required T Function(PetApiFindPetsByStatusResponse400 response) on400,
    required T Function(PetApiFindPetsByStatusResponse response) other,
  }) {
    return switch (this) {
      PetApiFindPetsByStatusResponse200 response => on200(response),
      PetApiFindPetsByStatusResponse400 response => on400(response),
      _ => other(this),
    };
  }

  static Future<PetApiFindPetsByStatusResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<PetApiFindPetsByStatusResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => PetApiFindPetsByStatusResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => PetApiFindPetsByStatusResponse400.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return PetApiFindPetsByStatusResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class PetApiFindPetsByStatusResponse200 extends PetApiFindPetsByStatusResponse {
  PetApiFindPetsByStatusResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(PetApiFindPetsByStatusResponse200ApplicationXml response) onApplicationXml,
    
    required T Function(PetApiFindPetsByStatusResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(PetApiFindPetsByStatusResponse200 response) other,
  }) {
    return switch (this) {
      
      PetApiFindPetsByStatusResponse200ApplicationXml response => onApplicationXml(response),
      
      PetApiFindPetsByStatusResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<PetApiFindPetsByStatusResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<PetApiFindPetsByStatusResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/xml')),
        () => PetApiFindPetsByStatusResponse200ApplicationXml.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => PetApiFindPetsByStatusResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return PetApiFindPetsByStatusResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}










/// Represent the response when content-type is application/xml.
class PetApiFindPetsByStatusResponse200ApplicationXml extends PetApiFindPetsByStatusResponse200 {
  final UndefinedWrapper<
    List<
        
            Pet
>
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Pet',
),
    
            
        
        
            
                Pet.$reflection
        
,
)
)
,
)
;

  /// The raw result of calling XmlDocumentFragment.parse
  final XmlDocumentFragment? rawXml;

  PetApiFindPetsByStatusResponse200ApplicationXml({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawXml,
  });

  static Future<PetApiFindPetsByStatusResponse200ApplicationXml> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'xml'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = XmlDocumentFragment.parse(serialized);
        // check if v can be deserialized to xml
        return PetApiFindPetsByStatusResponse200ApplicationXml(
          headers: response.headers,
          statusCode: response.statusCode,
          reasonPhrase: response.reasonPhrase,
          userContext: userContext,
          rawXml: v,
          
        );
      default:
    }
    return PetApiFindPetsByStatusResponse200ApplicationXml(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}








/// Represent the response when content-type is application/json.
class PetApiFindPetsByStatusResponse200ApplicationJson extends PetApiFindPetsByStatusResponse200 {
  final UndefinedWrapper<
    List<
        
            Pet
>
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Pet',
),
    
            
        
        
            
                Pet.$reflection
        
,
)
)
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  PetApiFindPetsByStatusResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<PetApiFindPetsByStatusResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
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
          return PetApiFindPetsByStatusResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return PetApiFindPetsByStatusResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return PetApiFindPetsByStatusResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}



class PetApiFindPetsByStatusResponse400 extends PetApiFindPetsByStatusResponse {
  PetApiFindPetsByStatusResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<PetApiFindPetsByStatusResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return PetApiFindPetsByStatusResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




@Deprecated('This operation has been deprecated')
class PetApiFindPetsByTagsResponse {
  PetApiFindPetsByTagsResponse({
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
    required T Function(PetApiFindPetsByTagsResponse200 response) on200,
    required T Function(PetApiFindPetsByTagsResponse400 response) on400,
    required T Function(PetApiFindPetsByTagsResponse response) other,
  }) {
    return switch (this) {
      PetApiFindPetsByTagsResponse200 response => on200(response),
      PetApiFindPetsByTagsResponse400 response => on400(response),
      _ => other(this),
    };
  }

  static Future<PetApiFindPetsByTagsResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<PetApiFindPetsByTagsResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => PetApiFindPetsByTagsResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => PetApiFindPetsByTagsResponse400.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return PetApiFindPetsByTagsResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



@Deprecated('This operation has been deprecated')
class PetApiFindPetsByTagsResponse200 extends PetApiFindPetsByTagsResponse {
  PetApiFindPetsByTagsResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(PetApiFindPetsByTagsResponse200ApplicationXml response) onApplicationXml,
    
    required T Function(PetApiFindPetsByTagsResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(PetApiFindPetsByTagsResponse200 response) other,
  }) {
    return switch (this) {
      
      PetApiFindPetsByTagsResponse200ApplicationXml response => onApplicationXml(response),
      
      PetApiFindPetsByTagsResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<PetApiFindPetsByTagsResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<PetApiFindPetsByTagsResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/xml')),
        () => PetApiFindPetsByTagsResponse200ApplicationXml.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => PetApiFindPetsByTagsResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return PetApiFindPetsByTagsResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}










/// Represent the response when content-type is application/xml.
@Deprecated('This operation has been deprecated')
class PetApiFindPetsByTagsResponse200ApplicationXml extends PetApiFindPetsByTagsResponse200 {
  final UndefinedWrapper<
    List<
        
            Pet
>
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Pet',
),
    
            
        
        
            
                Pet.$reflection
        
,
)
)
,
)
;

  /// The raw result of calling XmlDocumentFragment.parse
  final XmlDocumentFragment? rawXml;

  PetApiFindPetsByTagsResponse200ApplicationXml({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawXml,
  });

  static Future<PetApiFindPetsByTagsResponse200ApplicationXml> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'xml'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = XmlDocumentFragment.parse(serialized);
        // check if v can be deserialized to xml
        return PetApiFindPetsByTagsResponse200ApplicationXml(
          headers: response.headers,
          statusCode: response.statusCode,
          reasonPhrase: response.reasonPhrase,
          userContext: userContext,
          rawXml: v,
          
        );
      default:
    }
    return PetApiFindPetsByTagsResponse200ApplicationXml(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}








/// Represent the response when content-type is application/json.
@Deprecated('This operation has been deprecated')
class PetApiFindPetsByTagsResponse200ApplicationJson extends PetApiFindPetsByTagsResponse200 {
  final UndefinedWrapper<
    List<
        
            Pet
>
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Pet',
),
    
            
        
        
            
                Pet.$reflection
        
,
)
)
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  PetApiFindPetsByTagsResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<PetApiFindPetsByTagsResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
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
          return PetApiFindPetsByTagsResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return PetApiFindPetsByTagsResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return PetApiFindPetsByTagsResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}



@Deprecated('This operation has been deprecated')
class PetApiFindPetsByTagsResponse400 extends PetApiFindPetsByTagsResponse {
  PetApiFindPetsByTagsResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<PetApiFindPetsByTagsResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return PetApiFindPetsByTagsResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class PetApiGetPetByIdResponse {
  PetApiGetPetByIdResponse({
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
    required T Function(PetApiGetPetByIdResponse200 response) on200,
    required T Function(PetApiGetPetByIdResponse400 response) on400,
    required T Function(PetApiGetPetByIdResponse404 response) on404,
    required T Function(PetApiGetPetByIdResponse response) other,
  }) {
    return switch (this) {
      PetApiGetPetByIdResponse200 response => on200(response),
      PetApiGetPetByIdResponse400 response => on400(response),
      PetApiGetPetByIdResponse404 response => on404(response),
      _ => other(this),
    };
  }

  static Future<PetApiGetPetByIdResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<PetApiGetPetByIdResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => PetApiGetPetByIdResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => PetApiGetPetByIdResponse400.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'404'),
      () => PetApiGetPetByIdResponse404.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return PetApiGetPetByIdResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class PetApiGetPetByIdResponse200 extends PetApiGetPetByIdResponse {
  PetApiGetPetByIdResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(PetApiGetPetByIdResponse200ApplicationXml response) onApplicationXml,
    
    required T Function(PetApiGetPetByIdResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(PetApiGetPetByIdResponse200 response) other,
  }) {
    return switch (this) {
      
      PetApiGetPetByIdResponse200ApplicationXml response => onApplicationXml(response),
      
      PetApiGetPetByIdResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<PetApiGetPetByIdResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<PetApiGetPetByIdResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/xml')),
        () => PetApiGetPetByIdResponse200ApplicationXml.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => PetApiGetPetByIdResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return PetApiGetPetByIdResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/xml.
class PetApiGetPetByIdResponse200ApplicationXml extends PetApiGetPetByIdResponse200 {
  final UndefinedWrapper<
            Pet
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Pet',
),
    
            
        
        
            
                Pet.$reflection
        
,
)
;

  /// The raw result of calling XmlDocumentFragment.parse
  final XmlDocumentFragment? rawXml;

  PetApiGetPetByIdResponse200ApplicationXml({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawXml,
  });

  static Future<PetApiGetPetByIdResponse200ApplicationXml> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
  final encodingRules = <String, PropertyEncodingRule>{
      
    };

    final context = wireSerializationOptions.createSerializationContext(contentType);

    switch (contentType) {
      case MediaType(type: 'application', subtype: 'xml'):
        final encoding = OASNetworkingUtils.getEncodingOrDefault(contentType);
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = XmlDocumentFragment.parse(serialized);
        // check if v can be deserialized to xml
        return PetApiGetPetByIdResponse200ApplicationXml(
          headers: response.headers,
          statusCode: response.statusCode,
          reasonPhrase: response.reasonPhrase,
          userContext: userContext,
          rawXml: v,
          
        );
      default:
    }
    return PetApiGetPetByIdResponse200ApplicationXml(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}






/// Represent the response when content-type is application/json.
class PetApiGetPetByIdResponse200ApplicationJson extends PetApiGetPetByIdResponse200 {
  final UndefinedWrapper<
            Pet
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Pet',
),
    
            
        
        
            
                Pet.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  PetApiGetPetByIdResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<PetApiGetPetByIdResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
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
          return PetApiGetPetByIdResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return PetApiGetPetByIdResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return PetApiGetPetByIdResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}



class PetApiGetPetByIdResponse400 extends PetApiGetPetByIdResponse {
  PetApiGetPetByIdResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<PetApiGetPetByIdResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return PetApiGetPetByIdResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}





class PetApiGetPetByIdResponse404 extends PetApiGetPetByIdResponse {
  PetApiGetPetByIdResponse404({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<PetApiGetPetByIdResponse404> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return PetApiGetPetByIdResponse404(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class PetApiUpdatePetResponse {
  PetApiUpdatePetResponse({
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
    required T Function(PetApiUpdatePetResponse400 response) on400,
    required T Function(PetApiUpdatePetResponse404 response) on404,
    required T Function(PetApiUpdatePetResponse405 response) on405,
    required T Function(PetApiUpdatePetResponse response) other,
  }) {
    return switch (this) {
      PetApiUpdatePetResponse400 response => on400(response),
      PetApiUpdatePetResponse404 response => on404(response),
      PetApiUpdatePetResponse405 response => on405(response),
      _ => other(this),
    };
  }

  static Future<PetApiUpdatePetResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<PetApiUpdatePetResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'400'),
      () => PetApiUpdatePetResponse400.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'404'),
      () => PetApiUpdatePetResponse404.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'405'),
      () => PetApiUpdatePetResponse405.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return PetApiUpdatePetResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class PetApiUpdatePetResponse400 extends PetApiUpdatePetResponse {
  PetApiUpdatePetResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<PetApiUpdatePetResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return PetApiUpdatePetResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}





class PetApiUpdatePetResponse404 extends PetApiUpdatePetResponse {
  PetApiUpdatePetResponse404({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<PetApiUpdatePetResponse404> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return PetApiUpdatePetResponse404(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}





class PetApiUpdatePetResponse405 extends PetApiUpdatePetResponse {
  PetApiUpdatePetResponse405({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<PetApiUpdatePetResponse405> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return PetApiUpdatePetResponse405(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class PetApiUpdatePetWithFormResponse {
  PetApiUpdatePetWithFormResponse({
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
    required T Function(PetApiUpdatePetWithFormResponse405 response) on405,
    required T Function(PetApiUpdatePetWithFormResponse response) other,
  }) {
    return switch (this) {
      PetApiUpdatePetWithFormResponse405 response => on405(response),
      _ => other(this),
    };
  }

  static Future<PetApiUpdatePetWithFormResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<PetApiUpdatePetWithFormResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'405'),
      () => PetApiUpdatePetWithFormResponse405.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return PetApiUpdatePetWithFormResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class PetApiUpdatePetWithFormResponse405 extends PetApiUpdatePetWithFormResponse {
  PetApiUpdatePetWithFormResponse405({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });




  static Future<PetApiUpdatePetWithFormResponse405> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    return PetApiUpdatePetWithFormResponse405(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}




class PetApiUploadFileResponse {
  PetApiUploadFileResponse({
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
    required T Function(PetApiUploadFileResponse200 response) on200,
    required T Function(PetApiUploadFileResponse response) other,
  }) {
    return switch (this) {
      PetApiUploadFileResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<PetApiUploadFileResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<PetApiUploadFileResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => PetApiUploadFileResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return PetApiUploadFileResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class PetApiUploadFileResponse200 extends PetApiUploadFileResponse {
  PetApiUploadFileResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(PetApiUploadFileResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(PetApiUploadFileResponse200 response) other,
  }) {
    return switch (this) {
      
      PetApiUploadFileResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<PetApiUploadFileResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<PetApiUploadFileResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => PetApiUploadFileResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return PetApiUploadFileResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/json.
class PetApiUploadFileResponse200ApplicationJson extends PetApiUploadFileResponse200 {
  final UndefinedWrapper<
            ApiResponse
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                ApiResponse.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  PetApiUploadFileResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<PetApiUploadFileResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
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
          return PetApiUploadFileResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return PetApiUploadFileResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return PetApiUploadFileResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}


class PetApiUploadFileWithRequiredFileResponse {
  PetApiUploadFileWithRequiredFileResponse({
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
    required T Function(PetApiUploadFileWithRequiredFileResponse200 response) on200,
    required T Function(PetApiUploadFileWithRequiredFileResponse response) other,
  }) {
    return switch (this) {
      PetApiUploadFileWithRequiredFileResponse200 response => on200(response),
      _ => other(this),
    };
  }

  static Future<PetApiUploadFileWithRequiredFileResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final matchedResponse = <(PatternMatchResult, Future<PetApiUploadFileWithRequiredFileResponse> Function())> [
    (
      OASNetworkingUtils.matchesStatusCodePattern(response.statusCode, r'200'),
      () => PetApiUploadFileWithRequiredFileResponse200.fromResponse(response, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
    ),
    ].pickPrioritized();
    if (matchedResponse != null) {
      return matchedResponse();
    }
    return PetApiUploadFileWithRequiredFileResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



class PetApiUploadFileWithRequiredFileResponse200 extends PetApiUploadFileWithRequiredFileResponse {
  PetApiUploadFileWithRequiredFileResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
  });


  T split200<T>({
    
    required T Function(PetApiUploadFileWithRequiredFileResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(PetApiUploadFileWithRequiredFileResponse200 response) other,
  }) {
    return switch (this) {
      
      PetApiUploadFileWithRequiredFileResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<PetApiUploadFileWithRequiredFileResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      final matchedResponse = <(PatternMatchResult, Future<PetApiUploadFileWithRequiredFileResponse200> Function())>[
      (
        OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json')),
        () => PetApiUploadFileWithRequiredFileResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, userContext: userContext, wireSerializationOptions: wireSerializationOptions)
      ),
      ].pickPrioritized();
      if (matchedResponse != null) {
        return matchedResponse();
      }
    }
    return PetApiUploadFileWithRequiredFileResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}








/// Represent the response when content-type is application/json.
class PetApiUploadFileWithRequiredFileResponse200ApplicationJson extends PetApiUploadFileWithRequiredFileResponse200 {
  final UndefinedWrapper<
            ApiResponse
> body;

  static const bodyReflection = XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                ApiResponse.$reflection
        
,
)
;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  PetApiUploadFileWithRequiredFileResponse200ApplicationJson({
    this.body = const UndefinedWrapper.undefined(),
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.userContext,
    super.bodyBytesStream,
    this.rawJson,
  });

  static Future<PetApiUploadFileWithRequiredFileResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> userContext, required WireSerializationOptions wireSerializationOptions}) async {
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
          return PetApiUploadFileWithRequiredFileResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            body: UndefinedWrapper(res),
            
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return PetApiUploadFileWithRequiredFileResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            userContext: userContext,
            rawJson: v,
            
          );
        }
      default:
    }
    return PetApiUploadFileWithRequiredFileResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      userContext: userContext,
      bodyBytesStream: response.bodyBytesStream,
      
    );
  }
}





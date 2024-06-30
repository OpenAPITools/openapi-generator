// ignore_for_file: unnecessary_type_check

part of 'values_api.dart';


class ValuesApiGetSomeValuesResponse {
  ValuesApiGetSomeValuesResponse({
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
    required T Function(ValuesApiGetSomeValuesResponse200 response) on200,
    required T Function(ValuesApiGetSomeValuesResponse400 response) on400,
    required T Function(ValuesApiGetSomeValuesResponse response) other,
  }) {
    return switch (this) {
      ValuesApiGetSomeValuesResponse200 response => on200(response),
      ValuesApiGetSomeValuesResponse400 response => on400(response),
      _ => other(this),
    };
  }

  static Future<ValuesApiGetSomeValuesResponse> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    if (OASNetworkingUtils.matchesStatsuCodePattern(response.statusCode, r'200')) {
      return ValuesApiGetSomeValuesResponse200.fromResponse(response, context: context);
    }
    if (OASNetworkingUtils.matchesStatsuCodePattern(response.statusCode, r'400')) {
      return ValuesApiGetSomeValuesResponse400.fromResponse(response, context: context);
    }
    return ValuesApiGetSomeValuesResponse(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class ValuesApiGetSomeValuesResponse200 extends ValuesApiGetSomeValuesResponse {
  ValuesApiGetSomeValuesResponse200({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });

  T split200<T>({
    
    required T Function(ValuesApiGetSomeValuesResponse200ApplicationJson response) onApplicationJson,
    
    required T Function(ValuesApiGetSomeValuesResponse200 response) other,
  }) {
    return switch (this) {
      
      ValuesApiGetSomeValuesResponse200ApplicationJson response => onApplicationJson(response),
      
      _ => other(this),
    };
  }

  static Future<ValuesApiGetSomeValuesResponse200> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    final headers = response.headers;
    final contentTypeRaw = headers['Content-Type'];
    final contentTypeParsed = contentTypeRaw == null ? null : MediaType.parse(contentTypeRaw);
    if (contentTypeParsed != null) {
      if (OASNetworkingUtils.matchesContentTypePattern(contentTypeParsed, MediaType.parse(r'application/json'))) {
        return ValuesApiGetSomeValuesResponse200ApplicationJson.fromResponse(response, contentType: contentTypeParsed, context: context);
      }
    }
    return ValuesApiGetSomeValuesResponse200(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}



/// Represent the response when content-type is application/json.
class ValuesApiGetSomeValuesResponse200ApplicationJson extends ValuesApiGetSomeValuesResponse200 {
  final UndefinedWrapper<
            Variable
> body;

  /// The raw result of calling jsonDecode
  final Object? rawJson;

  ValuesApiGetSomeValuesResponse200ApplicationJson({
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

  static Future<ValuesApiGetSomeValuesResponse200ApplicationJson> fromResponse(HttpResponseBase response, {required MediaType contentType, required Map<String,dynamic> context}) async {
    final charset = contentType.parameters['charset'] ?? 'utf-8';
    final encoding = Encoding.getByName(charset) ?? utf8;
    switch (contentType) {
      
      case MediaType(type: 'application', subtype: 'json'):
        final serialized = await encoding.decodeStream(response.bodyBytesStream);
        final v = jsonDecode(serialized);
        if (v == null ? false :
(

    
            Variable.canDeserialize(v)
            
)) {
          final res = Variable.deserialize
(

    
            v


)


;
          return ValuesApiGetSomeValuesResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            body: UndefinedWrapper(res),
          );
        } else {
          // since we consumed the stream, we need to publish our read result.
          return ValuesApiGetSomeValuesResponse200ApplicationJson(
            headers: response.headers,
            statusCode: response.statusCode,
            reasonPhrase: response.reasonPhrase,
            context: context,
            rawJson: v,
          );
        }
      
      
    }
    return ValuesApiGetSomeValuesResponse200ApplicationJson(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}

class ValuesApiGetSomeValuesResponse400 extends ValuesApiGetSomeValuesResponse {
  ValuesApiGetSomeValuesResponse400({
    required super.headers,
    required super.statusCode,
    required super.reasonPhrase,
    required super.context,
    super.bodyBytesStream,
  });



  static Future<ValuesApiGetSomeValuesResponse400> fromResponse(HttpResponseBase response, {required Map<String,dynamic> context}) async {
    return ValuesApiGetSomeValuesResponse400(
      headers: response.headers,
      statusCode: response.statusCode,
      reasonPhrase: response.reasonPhrase,
      context: context,
      bodyBytesStream: response.bodyBytesStream,
    );
  }
}







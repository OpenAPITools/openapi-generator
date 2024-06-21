part of 'default_api.dart';


abstract class DefaultApiFakeAnyOfWIthSameErasureGetRequest {
  static const pathTemplate = r'/fake/anyOfWIthSameErasure';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;


  const DefaultApiFakeAnyOfWIthSameErasureGetRequest({
    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
  }) async {
    var resolvedPath = pathTemplate;
    var methodUri = Uri(path: resolvedPath);

    return baseUrl.replace(
      pathSegments: [
        ...baseUrl.pathSegments,
        ...methodUri.pathSegments,
      ],
      queryParameters: {
        ...baseUrl.queryParameters,
        ...methodUri.queryParameters,
        ...extraQueryParameters,
      },
    );
  }

  Future<Map<String, String>> getResolvedHeaders({
    Map<String, dynamic> context = const {},
  }) async {

    final cookieParts = <String,String>{
      ...extraCookies,
    };

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),

    };
  }


  Future<HttpRequestBase> createHttpRequest({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
  }) async {
    final futures = [
      getResolvedUri(
        context: context,
        baseUrl: baseUrl,
      ),
      getResolvedHeaders(context: context),
    ];
    final futureResults = await Future.wait(futures);
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: Stream.empty(),
      context: context,
    );
  }
}


class DefaultApiFakeAnyOfWIthSameErasureGetResponse {
}


abstract class DefaultApiFakeOneOfWIthSameErasureGetRequest {
  static const pathTemplate = r'/fake/oneOfWIthSameErasure';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;


  const DefaultApiFakeOneOfWIthSameErasureGetRequest({
    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
  }) async {
    var resolvedPath = pathTemplate;
    var methodUri = Uri(path: resolvedPath);

    return baseUrl.replace(
      pathSegments: [
        ...baseUrl.pathSegments,
        ...methodUri.pathSegments,
      ],
      queryParameters: {
        ...baseUrl.queryParameters,
        ...methodUri.queryParameters,
        ...extraQueryParameters,
      },
    );
  }

  Future<Map<String, String>> getResolvedHeaders({
    Map<String, dynamic> context = const {},
  }) async {

    final cookieParts = <String,String>{
      ...extraCookies,
    };

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),

    };
  }


  Future<HttpRequestBase> createHttpRequest({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
  }) async {
    final futures = [
      getResolvedUri(
        context: context,
        baseUrl: baseUrl,
      ),
      getResolvedHeaders(context: context),
    ];
    final futureResults = await Future.wait(futures);
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: Stream.empty(),
      context: context,
    );
  }
}


class DefaultApiFakeOneOfWIthSameErasureGetResponse {
}


abstract class DefaultApiFooGetRequest {
  static const pathTemplate = r'/foo';
  static String method = r'GET';

  final Map<String, String> extraHeaders;
  final Map<String, String> extraCookies;
  final Map<String, Object /* String | List<String> */> extraQueryParameters;


  const DefaultApiFooGetRequest({
    this.extraHeaders = const {},
    this.extraQueryParameters = const {},
    this.extraCookies = const {},
  });

  Future<Uri> getResolvedUri({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
  }) async {
    var resolvedPath = pathTemplate;
    var methodUri = Uri(path: resolvedPath);

    return baseUrl.replace(
      pathSegments: [
        ...baseUrl.pathSegments,
        ...methodUri.pathSegments,
      ],
      queryParameters: {
        ...baseUrl.queryParameters,
        ...methodUri.queryParameters,
        ...extraQueryParameters,
      },
    );
  }

  Future<Map<String, String>> getResolvedHeaders({
    Map<String, dynamic> context = const {},
  }) async {

    final cookieParts = <String,String>{
      ...extraCookies,
    };

    return {
      if (cookieParts.isNotEmpty)
        'Cookie': cookieParts.entries.map((e) => '${e.key}=${e.value}').join('; '),

    };
  }


  Future<HttpRequestBase> createHttpRequest({
    required Uri baseUrl,
    Map<String, dynamic> context = const {},
  }) async {
    final futures = [
      getResolvedUri(
        context: context,
        baseUrl: baseUrl,
      ),
      getResolvedHeaders(context: context),
    ];
    final futureResults = await Future.wait(futures);
    // Add any path/query parameters to the knownUrl.
    return HttpRequestBase.stream(
      url: futureResults[0] as Uri,
      headers: futureResults[1] as Map<String, String>,
      method: method,
      bodyBytesStream: Stream.empty(),
      context: context,
    );
  }
}


class DefaultApiFooGetResponse {
}


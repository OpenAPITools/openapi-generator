import 'dart:async';
import 'dart:convert';
import 'package:dio/dio.dart';
import 'package:built_value/serializer.dart';

import 'package:openapi/model/client.dart';

class FakeClassnameTags123Api {
    final Dio _dio;
    Serializers _serializers;

    FakeClassnameTags123Api(this._dio, this._serializers);

    /// To test class name in snake case
    ///
    /// To test class name in snake case
    Future<Response<Client>> testClassname(
        Client client, { 
        CancelToken cancelToken,
        Map<String, String> headers,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final String _path = '/fake_classname_test';

        final Map<String, dynamic> queryParams = {};
        final Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        final List<String> contentTypes = [
            'application/json',
        ];

        final serializedBody = _serializers.serialize(client);
        final jsonclient = json.encode(serializedBody);
        bodyData = jsonclient;

        return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'patch'.toUpperCase(),
                headers: headerParams,
                extra: {
                    'secure': [
                        {
                            'type': 'apiKey',
                            'name': 'api_key_query',
                            'keyName': 'api_key_query',
                            'where': 'query',
                        },
                    ],
                },
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        ).then((response) {
            final serializer = _serializers.serializerForType(Client);
            final data = _serializers.deserializeWith<Client>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

            return Response<Client>(
                data: data,
                headers: response.headers,
                request: response.request,
                redirects: response.redirects,
                statusCode: response.statusCode,
                statusMessage: response.statusMessage,
                extra: response.extra,
            );
        });
    }

}

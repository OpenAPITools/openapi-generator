import 'dart:async';
import 'dart:convert';
import 'package:dio/dio.dart';
import 'package:built_value/serializer.dart';

import 'package:openapi/model/model_client.dart';

class FakeClassnameTags123Api {
    final Dio _dio;
    Serializers _serializers;

    FakeClassnameTags123Api(this._dio, this._serializers);

    /// To test class name in snake case
    ///
    /// To test class name in snake case
    Future<Response<ModelClient>> testClassname(
        ModelClient modelClient, { 
        CancelToken cancelToken,
        Map<String, dynamic> headers,
        Map<String, dynamic> extra,
        ValidateStatus validateStatus,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final String _path = '/fake_classname_test';

        final Map<String, dynamic> queryParams = {};
        final Map<String, dynamic> headerParams = {
            if (headers != null) ...headers,
        };
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        final List<String> contentTypes = [
            'application/json',
        ];

        final bodySerializer = _serializers.serializerForType(ModelClient);
        final serializedBody = _serializers.serializeWith(bodySerializer, modelClient);
        final jsonmodelClient = json.encode(serializedBody);
        bodyData = jsonmodelClient;

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
                    if (extra != null) ...extra,
                },
                validateStatus: validateStatus,
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        ).then((response) {
            final serializer = _serializers.serializerForType(ModelClient);
            final data = _serializers.deserializeWith<ModelClient>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

            return Response<ModelClient>(
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

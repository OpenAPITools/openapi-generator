import 'dart:async';
import 'dart:convert';
import 'package:dio/dio.dart';
import 'package:built_value/serializer.dart';

import 'package:openapi/model/inline_response_default.dart';

class DefaultApi {
    final Dio _dio;
    Serializers _serializers;

    DefaultApi(this._dio, this._serializers);

    /// 
    ///
    /// 
    Future<Response<InlineResponseDefault>> fooGet({ 
        CancelToken cancelToken,
        Map<String, String> headers,
        ProgressCallback onSendProgress,
        ProgressCallback onReceiveProgress,
    }) async {
        final String _path = '/foo';

        final Map<String, dynamic> queryParams = {};
        final Map<String, String> headerParams = Map.from(headers ?? {});
        dynamic bodyData;

        queryParams.removeWhere((key, value) => value == null);
        headerParams.removeWhere((key, value) => value == null);

        final List<String> contentTypes = [];

        return _dio.request(
            _path,
            queryParameters: queryParams,
            data: bodyData,
            options: Options(
                method: 'get'.toUpperCase(),
                headers: headerParams,
                extra: {
                    'secure': [],
                },
                contentType: contentTypes.isNotEmpty ? contentTypes[0] : 'application/json',
            ),
            cancelToken: cancelToken,
            onSendProgress: onSendProgress,
            onReceiveProgress: onReceiveProgress,
        ).then((response) {
            final serializer = _serializers.serializerForType(InlineResponseDefault);
            final data = _serializers.deserializeWith<InlineResponseDefault>(serializer, response.data is String ? jsonDecode(response.data) : response.data);

            return Response<InlineResponseDefault>(
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

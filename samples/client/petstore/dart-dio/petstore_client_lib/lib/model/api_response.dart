//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'api_response.g.dart';

abstract class ApiResponse implements Built<ApiResponse, ApiResponseBuilder> {

    @nullable
    @BuiltValueField(wireName: r'code')
    int get code;

    @nullable
    @BuiltValueField(wireName: r'type')
    String get type;

    @nullable
    @BuiltValueField(wireName: r'message')
    String get message;

    // Boilerplate code needed to wire-up generated code
    ApiResponse._();

    static void _initializeBuilder(ApiResponseBuilder b) => b;

    factory ApiResponse([void updates(ApiResponseBuilder b)]) = _$ApiResponse;
    static Serializer<ApiResponse> get serializer => _$apiResponseSerializer;
}


//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model200_response.g.dart';

abstract class Model200Response implements Built<Model200Response, Model200ResponseBuilder> {

    @nullable
    @BuiltValueField(wireName: r'name')
    int get name;

    @nullable
    @BuiltValueField(wireName: r'class')
    String get class_;

    // Boilerplate code needed to wire-up generated code
    Model200Response._();

    static void _initializeBuilder(Model200ResponseBuilder b) => b;

    factory Model200Response([void updates(Model200ResponseBuilder b)]) = _$Model200Response;
    static Serializer<Model200Response> get serializer => _$model200ResponseSerializer;
}


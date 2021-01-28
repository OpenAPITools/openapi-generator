//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:openapi/model/foo.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'inline_response_default.g.dart';

abstract class InlineResponseDefault implements Built<InlineResponseDefault, InlineResponseDefaultBuilder> {

    @nullable
    @BuiltValueField(wireName: r'string')
    Foo get string;

    // Boilerplate code needed to wire-up generated code
    InlineResponseDefault._();

    static void _initializeBuilder(InlineResponseDefaultBuilder b) => b;

    factory InlineResponseDefault([void updates(InlineResponseDefaultBuilder b)]) = _$InlineResponseDefault;
    static Serializer<InlineResponseDefault> get serializer => _$inlineResponseDefaultSerializer;
}


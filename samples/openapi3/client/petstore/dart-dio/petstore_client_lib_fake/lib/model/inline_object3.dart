import 'dart:typed_data';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'inline_object3.g.dart';

abstract class InlineObject3 implements Built<InlineObject3, InlineObject3Builder> {

    /* None */
    @nullable
    @BuiltValueField(wireName: r'integer')
    int get integer;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'int32')
    int get int32;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'int64')
    int get int64;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'number')
    num get number;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'float')
    double get float;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'double')
    double get double_;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'string')
    String get string;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'pattern_without_delimiter')
    String get patternWithoutDelimiter;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'byte')
    String get byte;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'binary')
    Uint8List get binary;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'date')
    DateTime get date;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'dateTime')
    DateTime get dateTime;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'password')
    String get password;
    /* None */
    @nullable
    @BuiltValueField(wireName: r'callback')
    String get callback;

    // Boilerplate code needed to wire-up generated code
    InlineObject3._();

    factory InlineObject3([updates(InlineObject3Builder b)]) = _$InlineObject3;
    static Serializer<InlineObject3> get serializer => _$inlineObject3Serializer;
}


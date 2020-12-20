import 'dart:typed_data';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'inline_object1.g.dart';

abstract class InlineObject1 implements Built<InlineObject1, InlineObject1Builder> {

    /// Additional data to pass to server
    @nullable
    @BuiltValueField(wireName: r'additionalMetadata')
    String get additionalMetadata;

    /// file to upload
    @nullable
    @BuiltValueField(wireName: r'file')
    Uint8List get file;

    // Boilerplate code needed to wire-up generated code
    InlineObject1._();

    static void _initializeBuilder(InlineObject1Builder b) => b;

    factory InlineObject1([updates(InlineObject1Builder b)]) = _$InlineObject1;
    static Serializer<InlineObject1> get serializer => _$inlineObject1Serializer;
}


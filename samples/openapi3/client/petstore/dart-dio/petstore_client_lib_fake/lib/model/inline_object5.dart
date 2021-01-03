import 'dart:typed_data';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'inline_object5.g.dart';

abstract class InlineObject5 implements Built<InlineObject5, InlineObject5Builder> {

    /// Additional data to pass to server
    @nullable
    @BuiltValueField(wireName: r'additionalMetadata')
    String get additionalMetadata;

    /// file to upload
    @nullable
    @BuiltValueField(wireName: r'requiredFile')
    Uint8List get requiredFile;

    // Boilerplate code needed to wire-up generated code
    InlineObject5._();

    static void _initializeBuilder(InlineObject5Builder b) => b;

    factory InlineObject5([updates(InlineObject5Builder b)]) = _$InlineObject5;
    static Serializer<InlineObject5> get serializer => _$inlineObject5Serializer;
}


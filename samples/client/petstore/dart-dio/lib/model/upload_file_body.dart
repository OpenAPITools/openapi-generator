            import 'dart:typed_data';
        import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'upload_file_body.g.dart';

abstract class UploadFileBody implements Built<UploadFileBody, UploadFileBodyBuilder> {

    /* Additional data to pass to server */
        @nullable
    @BuiltValueField(wireName: r'additionalMetadata')
    String get additionalMetadata;
    /* file to upload */
        @nullable
    @BuiltValueField(wireName: r'file')
    Uint8List get file;

    // Boilerplate code needed to wire-up generated code
    UploadFileBody._();

    factory UploadFileBody([updates(UploadFileBodyBuilder b)]) = _$UploadFileBody;
    static Serializer<UploadFileBody> get serializer => _$uploadFileBodySerializer;
}


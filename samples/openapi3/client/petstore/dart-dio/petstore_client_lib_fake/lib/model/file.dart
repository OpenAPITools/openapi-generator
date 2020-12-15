import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'file.g.dart';

abstract class File implements Built<File, FileBuilder> {

    /// Test capitalization
    @nullable
    @BuiltValueField(wireName: r'sourceURI')
    String get sourceURI;

    // Boilerplate code needed to wire-up generated code
    File._();

    factory File([updates(FileBuilder b)]) = _$File;
    static Serializer<File> get serializer => _$fileSerializer;
}


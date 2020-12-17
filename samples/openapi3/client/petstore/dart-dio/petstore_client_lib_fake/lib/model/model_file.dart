import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_file.g.dart';

abstract class ModelFile implements Built<ModelFile, ModelFileBuilder> {

    /// Test capitalization
    @nullable
    @BuiltValueField(wireName: r'sourceURI')
    String get sourceURI;

    // Boilerplate code needed to wire-up generated code
    ModelFile._();

    static void _initializeBuilder(ModelFileBuilder b) => b;

    factory ModelFile([updates(ModelFileBuilder b)]) = _$ModelFile;
    static Serializer<ModelFile> get serializer => _$modelFileSerializer;
}


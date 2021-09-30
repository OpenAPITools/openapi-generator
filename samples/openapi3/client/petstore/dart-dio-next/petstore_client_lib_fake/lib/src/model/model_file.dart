//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_file.g.dart';

/// Must be named `File` for test.
///
/// Properties:
/// * [sourceURI] - Test capitalization
abstract class ModelFile implements Built<ModelFile, ModelFileBuilder> {
    /// Test capitalization
    @BuiltValueField(wireName: r'sourceURI')
    String? get sourceURI;

    ModelFile._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(ModelFileBuilder b) => b;

    factory ModelFile([void updates(ModelFileBuilder b)]) = _$ModelFile;

    @BuiltValueSerializer(custom: true)
    static Serializer<ModelFile> get serializer => _$ModelFileSerializer();
}

class _$ModelFileSerializer implements StructuredSerializer<ModelFile> {
    @override
    final Iterable<Type> types = const [ModelFile, _$ModelFile];

    @override
    final String wireName = r'ModelFile';

    @override
    Iterable<Object?> serialize(Serializers serializers, ModelFile object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.sourceURI != null) {
            result
                ..add(r'sourceURI')
                ..add(serializers.serialize(object.sourceURI,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    ModelFile deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = ModelFileBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'sourceURI':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.sourceURI = valueDes;
                    break;
            }
        }
        return result.build();
    }
}


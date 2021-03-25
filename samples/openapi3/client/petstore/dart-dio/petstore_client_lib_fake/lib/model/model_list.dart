//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_list.g.dart';

abstract class ModelList implements Built<ModelList, ModelListBuilder> {

    @nullable
    @BuiltValueField(wireName: r'123-list')
    String get n123list;

    ModelList._();

    static void _initializeBuilder(ModelListBuilder b) => b;

    factory ModelList([void updates(ModelListBuilder b)]) = _$ModelList;

    @BuiltValueSerializer(custom: true)
    static Serializer<ModelList> get serializer => _$ModelListSerializer();
}

class _$ModelListSerializer implements StructuredSerializer<ModelList> {

    @override
    final Iterable<Type> types = const [ModelList, _$ModelList];
    @override
    final String wireName = r'ModelList';

    @override
    Iterable<Object> serialize(Serializers serializers, ModelList object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.n123list != null) {
            result
                ..add(r'123-list')
                ..add(serializers.serialize(object.n123list,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    ModelList deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = ModelListBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'123-list':
                    result.n123list = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
            }
        }
        return result.build();
    }
}


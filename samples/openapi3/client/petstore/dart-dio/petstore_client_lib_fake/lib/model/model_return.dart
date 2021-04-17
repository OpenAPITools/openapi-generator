//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_return.g.dart';

abstract class ModelReturn implements Built<ModelReturn, ModelReturnBuilder> {

    @nullable
    @BuiltValueField(wireName: r'return')
    int get return_;

    ModelReturn._();

    static void _initializeBuilder(ModelReturnBuilder b) => b;

    factory ModelReturn([void updates(ModelReturnBuilder b)]) = _$ModelReturn;

    @BuiltValueSerializer(custom: true)
    static Serializer<ModelReturn> get serializer => _$ModelReturnSerializer();
}

class _$ModelReturnSerializer implements StructuredSerializer<ModelReturn> {

    @override
    final Iterable<Type> types = const [ModelReturn, _$ModelReturn];
    @override
    final String wireName = r'ModelReturn';

    @override
    Iterable<Object> serialize(Serializers serializers, ModelReturn object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.return_ != null) {
            result
                ..add(r'return')
                ..add(serializers.serialize(object.return_,
                    specifiedType: const FullType(int)));
        }
        return result;
    }

    @override
    ModelReturn deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = ModelReturnBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'return':
                    result.return_ = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
            }
        }
        return result.build();
    }
}


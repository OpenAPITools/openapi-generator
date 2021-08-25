//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_return.g.dart';

/// Model for testing reserved words
///
/// Properties:
/// * [return_] 
abstract class ModelReturn implements Built<ModelReturn, ModelReturnBuilder> {
    @BuiltValueField(wireName: r'return')
    int? get return_;

    ModelReturn._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(ModelReturnBuilder b) => b;

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
    Iterable<Object?> serialize(Serializers serializers, ModelReturn object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.return_ != null) {
            result
                ..add(r'return')
                ..add(serializers.serialize(object.return_,
                    specifiedType: const FullType(int)));
        }
        return result;
    }

    @override
    ModelReturn deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = ModelReturnBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'return':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    result.return_ = valueDes;
                    break;
            }
        }
        return result.build();
    }
}


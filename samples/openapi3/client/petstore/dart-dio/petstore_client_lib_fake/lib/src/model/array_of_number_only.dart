//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'array_of_number_only.g.dart';

/// ArrayOfNumberOnly
///
/// Properties:
/// * [arrayNumber] 
abstract class ArrayOfNumberOnly implements Built<ArrayOfNumberOnly, ArrayOfNumberOnlyBuilder> {
    @BuiltValueField(wireName: r'ArrayNumber')
    BuiltList<num>? get arrayNumber;

    ArrayOfNumberOnly._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(ArrayOfNumberOnlyBuilder b) => b;

    factory ArrayOfNumberOnly([void updates(ArrayOfNumberOnlyBuilder b)]) = _$ArrayOfNumberOnly;

    @BuiltValueSerializer(custom: true)
    static Serializer<ArrayOfNumberOnly> get serializer => _$ArrayOfNumberOnlySerializer();
}

class _$ArrayOfNumberOnlySerializer implements StructuredSerializer<ArrayOfNumberOnly> {
    @override
    final Iterable<Type> types = const [ArrayOfNumberOnly, _$ArrayOfNumberOnly];

    @override
    final String wireName = r'ArrayOfNumberOnly';

    @override
    Iterable<Object?> serialize(Serializers serializers, ArrayOfNumberOnly object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.arrayNumber != null) {
            result
                ..add(r'ArrayNumber')
                ..add(serializers.serialize(object.arrayNumber,
                    specifiedType: const FullType(BuiltList, [FullType(num)])));
        }
        return result;
    }

    @override
    ArrayOfNumberOnly deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = ArrayOfNumberOnlyBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'ArrayNumber':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(num)])) as BuiltList<num>;
                    result.arrayNumber.replace(valueDes);
                    break;
            }
        }
        return result.build();
    }
}


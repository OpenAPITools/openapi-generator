//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'number_only.g.dart';

/// NumberOnly
///
/// Properties:
/// * [justNumber] 
abstract class NumberOnly implements Built<NumberOnly, NumberOnlyBuilder> {
    @BuiltValueField(wireName: r'JustNumber')
    num? get justNumber;

    NumberOnly._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(NumberOnlyBuilder b) => b;

    factory NumberOnly([void updates(NumberOnlyBuilder b)]) = _$NumberOnly;

    @BuiltValueSerializer(custom: true)
    static Serializer<NumberOnly> get serializer => _$NumberOnlySerializer();
}

class _$NumberOnlySerializer implements StructuredSerializer<NumberOnly> {
    @override
    final Iterable<Type> types = const [NumberOnly, _$NumberOnly];

    @override
    final String wireName = r'NumberOnly';

    @override
    Iterable<Object?> serialize(Serializers serializers, NumberOnly object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.justNumber != null) {
            result
                ..add(r'JustNumber')
                ..add(serializers.serialize(object.justNumber,
                    specifiedType: const FullType(num)));
        }
        return result;
    }

    @override
    NumberOnly deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = NumberOnlyBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'JustNumber':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(num)) as num;
                    result.justNumber = valueDes;
                    break;
            }
        }
        return result.build();
    }
}


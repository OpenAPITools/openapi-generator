//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'has_only_read_only.g.dart';

/// HasOnlyReadOnly
///
/// Properties:
/// * [bar] 
/// * [foo] 
abstract class HasOnlyReadOnly implements Built<HasOnlyReadOnly, HasOnlyReadOnlyBuilder> {
    @BuiltValueField(wireName: r'bar')
    String? get bar;

    @BuiltValueField(wireName: r'foo')
    String? get foo;

    HasOnlyReadOnly._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(HasOnlyReadOnlyBuilder b) => b;

    factory HasOnlyReadOnly([void updates(HasOnlyReadOnlyBuilder b)]) = _$HasOnlyReadOnly;

    @BuiltValueSerializer(custom: true)
    static Serializer<HasOnlyReadOnly> get serializer => _$HasOnlyReadOnlySerializer();
}

class _$HasOnlyReadOnlySerializer implements StructuredSerializer<HasOnlyReadOnly> {
    @override
    final Iterable<Type> types = const [HasOnlyReadOnly, _$HasOnlyReadOnly];

    @override
    final String wireName = r'HasOnlyReadOnly';

    @override
    Iterable<Object?> serialize(Serializers serializers, HasOnlyReadOnly object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.bar != null) {
            result
                ..add(r'bar')
                ..add(serializers.serialize(object.bar,
                    specifiedType: const FullType(String)));
        }
        if (object.foo != null) {
            result
                ..add(r'foo')
                ..add(serializers.serialize(object.foo,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    HasOnlyReadOnly deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = HasOnlyReadOnlyBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'bar':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.bar = valueDes;
                    break;
                case r'foo':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.foo = valueDes;
                    break;
            }
        }
        return result.build();
    }
}


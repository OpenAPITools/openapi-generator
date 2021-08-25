//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'foo.g.dart';

/// Foo
///
/// Properties:
/// * [bar] 
abstract class Foo implements Built<Foo, FooBuilder> {
    @BuiltValueField(wireName: r'bar')
    String? get bar;

    Foo._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(FooBuilder b) => b
        ..bar = 'bar';

    factory Foo([void updates(FooBuilder b)]) = _$Foo;

    @BuiltValueSerializer(custom: true)
    static Serializer<Foo> get serializer => _$FooSerializer();
}

class _$FooSerializer implements StructuredSerializer<Foo> {
    @override
    final Iterable<Type> types = const [Foo, _$Foo];

    @override
    final String wireName = r'Foo';

    @override
    Iterable<Object?> serialize(Serializers serializers, Foo object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.bar != null) {
            result
                ..add(r'bar')
                ..add(serializers.serialize(object.bar,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    Foo deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = FooBuilder();

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
            }
        }
        return result.build();
    }
}


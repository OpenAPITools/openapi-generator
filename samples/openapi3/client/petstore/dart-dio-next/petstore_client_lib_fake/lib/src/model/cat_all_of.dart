//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'cat_all_of.g.dart';

/// CatAllOf
///
/// Properties:
/// * [declawed] 
abstract class CatAllOf implements Built<CatAllOf, CatAllOfBuilder> {
    @BuiltValueField(wireName: r'declawed')
    bool? get declawed;

    CatAllOf._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(CatAllOfBuilder b) => b;

    factory CatAllOf([void updates(CatAllOfBuilder b)]) = _$CatAllOf;

    @BuiltValueSerializer(custom: true)
    static Serializer<CatAllOf> get serializer => _$CatAllOfSerializer();
}

class _$CatAllOfSerializer implements StructuredSerializer<CatAllOf> {
    @override
    final Iterable<Type> types = const [CatAllOf, _$CatAllOf];

    @override
    final String wireName = r'CatAllOf';

    @override
    Iterable<Object?> serialize(Serializers serializers, CatAllOf object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.declawed != null) {
            result
                ..add(r'declawed')
                ..add(serializers.serialize(object.declawed,
                    specifiedType: const FullType(bool)));
        }
        return result;
    }

    @override
    CatAllOf deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = CatAllOfBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'declawed':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(bool)) as bool;
                    result.declawed = valueDes;
                    break;
            }
        }
        return result.build();
    }
}


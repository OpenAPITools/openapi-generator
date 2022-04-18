//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:openapi/src/model/single_ref_type.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'all_of_with_single_ref_single_ref_type.g.dart';

/// AllOfWithSingleRefSingleRefType
abstract class AllOfWithSingleRefSingleRefType implements Built<AllOfWithSingleRefSingleRefType, AllOfWithSingleRefSingleRefTypeBuilder> {
    AllOfWithSingleRefSingleRefType._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(AllOfWithSingleRefSingleRefTypeBuilder b) => b;

    factory AllOfWithSingleRefSingleRefType([void updates(AllOfWithSingleRefSingleRefTypeBuilder b)]) = _$AllOfWithSingleRefSingleRefType;

    @BuiltValueSerializer(custom: true)
    static Serializer<AllOfWithSingleRefSingleRefType> get serializer => _$AllOfWithSingleRefSingleRefTypeSerializer();
}

class _$AllOfWithSingleRefSingleRefTypeSerializer implements StructuredSerializer<AllOfWithSingleRefSingleRefType> {
    @override
    final Iterable<Type> types = const [AllOfWithSingleRefSingleRefType, _$AllOfWithSingleRefSingleRefType];

    @override
    final String wireName = r'AllOfWithSingleRefSingleRefType';

    @override
    Iterable<Object?> serialize(Serializers serializers, AllOfWithSingleRefSingleRefType object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        return result;
    }

    @override
    AllOfWithSingleRefSingleRefType deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = AllOfWithSingleRefSingleRefTypeBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
            }
        }
        return result.build();
    }
}


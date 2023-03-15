//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:openapi/src/model/single_ref_type.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'all_of_with_single_ref.g.dart';

/// AllOfWithSingleRef
///
/// Properties:
/// * [username] 
/// * [singleRefType] 
abstract class AllOfWithSingleRef implements Built<AllOfWithSingleRef, AllOfWithSingleRefBuilder> {
    @BuiltValueField(wireName: r'username')
    String? get username;

    @BuiltValueField(wireName: r'SingleRefType')
    SingleRefType? get singleRefType;

    AllOfWithSingleRef._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(AllOfWithSingleRefBuilder b) => b;

    factory AllOfWithSingleRef([void updates(AllOfWithSingleRefBuilder b)]) = _$AllOfWithSingleRef;

    @BuiltValueSerializer(custom: true)
    static Serializer<AllOfWithSingleRef> get serializer => _$AllOfWithSingleRefSerializer();
}

class _$AllOfWithSingleRefSerializer implements StructuredSerializer<AllOfWithSingleRef> {
    @override
    final Iterable<Type> types = const [AllOfWithSingleRef, _$AllOfWithSingleRef];

    @override
    final String wireName = r'AllOfWithSingleRef';

    @override
    Iterable<Object?> serialize(Serializers serializers, AllOfWithSingleRef object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.username != null) {
            result
                ..add(r'username')
                ..add(serializers.serialize(object.username,
                    specifiedType: const FullType(String)));
        }
        if (object.singleRefType != null) {
            result
                ..add(r'SingleRefType')
                ..add(serializers.serialize(object.singleRefType,
                    specifiedType: const FullType.nullable(SingleRefType)));
        }
        return result;
    }

    @override
    AllOfWithSingleRef deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = AllOfWithSingleRefBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'username':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.username = valueDes;
                    break;
                case r'SingleRefType':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType.nullable(SingleRefType)) as SingleRefType?;
                    if (valueDes == null) continue;
                    result.singleRefType = valueDes;
                    break;
            }
        }
        return result.build();
    }
}


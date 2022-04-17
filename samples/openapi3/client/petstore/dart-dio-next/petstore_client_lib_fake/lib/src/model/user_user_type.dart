//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:openapi/src/model/user_type.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'user_user_type.g.dart';

/// UserUserType
abstract class UserUserType implements Built<UserUserType, UserUserTypeBuilder> {
    UserUserType._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(UserUserTypeBuilder b) => b;

    factory UserUserType([void updates(UserUserTypeBuilder b)]) = _$UserUserType;

    @BuiltValueSerializer(custom: true)
    static Serializer<UserUserType> get serializer => _$UserUserTypeSerializer();
}

class _$UserUserTypeSerializer implements StructuredSerializer<UserUserType> {
    @override
    final Iterable<Type> types = const [UserUserType, _$UserUserType];

    @override
    final String wireName = r'UserUserType';

    @override
    Iterable<Object?> serialize(Serializers serializers, UserUserType object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        return result;
    }

    @override
    UserUserType deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = UserUserTypeBuilder();

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


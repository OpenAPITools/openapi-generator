//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:openapi/src/model/outer_enum_integer.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'outer_object_with_enum_property.g.dart';

/// OuterObjectWithEnumProperty
///
/// Properties:
/// * [value] 
abstract class OuterObjectWithEnumProperty implements Built<OuterObjectWithEnumProperty, OuterObjectWithEnumPropertyBuilder> {
    @BuiltValueField(wireName: r'value')
    OuterEnumInteger get value;
    // enum valueEnum {  0,  1,  2,  };

    OuterObjectWithEnumProperty._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(OuterObjectWithEnumPropertyBuilder b) => b;

    factory OuterObjectWithEnumProperty([void updates(OuterObjectWithEnumPropertyBuilder b)]) = _$OuterObjectWithEnumProperty;

    @BuiltValueSerializer(custom: true)
    static Serializer<OuterObjectWithEnumProperty> get serializer => _$OuterObjectWithEnumPropertySerializer();
}

class _$OuterObjectWithEnumPropertySerializer implements StructuredSerializer<OuterObjectWithEnumProperty> {
    @override
    final Iterable<Type> types = const [OuterObjectWithEnumProperty, _$OuterObjectWithEnumProperty];

    @override
    final String wireName = r'OuterObjectWithEnumProperty';

    @override
    Iterable<Object?> serialize(Serializers serializers, OuterObjectWithEnumProperty object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        result
            ..add(r'value')
            ..add(serializers.serialize(object.value,
                specifiedType: const FullType(OuterEnumInteger)));
        return result;
    }

    @override
    OuterObjectWithEnumProperty deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = OuterObjectWithEnumPropertyBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'value':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(OuterEnumInteger)) as OuterEnumInteger;
                    result.value = valueDes;
                    break;
            }
        }
        return result.build();
    }
}


//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/deprecated_object.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'object_with_deprecated_fields.g.dart';

/// ObjectWithDeprecatedFields
///
/// Properties:
/// * [uuid] 
/// * [id] 
/// * [deprecatedRef] 
/// * [bars] 
abstract class ObjectWithDeprecatedFields implements Built<ObjectWithDeprecatedFields, ObjectWithDeprecatedFieldsBuilder> {
    @BuiltValueField(wireName: r'uuid')
    String? get uuid;

    @BuiltValueField(wireName: r'id')
    num? get id;

    @BuiltValueField(wireName: r'deprecatedRef')
    DeprecatedObject? get deprecatedRef;

    @BuiltValueField(wireName: r'bars')
    BuiltList<String>? get bars;

    ObjectWithDeprecatedFields._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(ObjectWithDeprecatedFieldsBuilder b) => b;

    factory ObjectWithDeprecatedFields([void updates(ObjectWithDeprecatedFieldsBuilder b)]) = _$ObjectWithDeprecatedFields;

    @BuiltValueSerializer(custom: true)
    static Serializer<ObjectWithDeprecatedFields> get serializer => _$ObjectWithDeprecatedFieldsSerializer();
}

class _$ObjectWithDeprecatedFieldsSerializer implements StructuredSerializer<ObjectWithDeprecatedFields> {
    @override
    final Iterable<Type> types = const [ObjectWithDeprecatedFields, _$ObjectWithDeprecatedFields];

    @override
    final String wireName = r'ObjectWithDeprecatedFields';

    @override
    Iterable<Object?> serialize(Serializers serializers, ObjectWithDeprecatedFields object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.uuid != null) {
            result
                ..add(r'uuid')
                ..add(serializers.serialize(object.uuid,
                    specifiedType: const FullType(String)));
        }
        if (object.id != null) {
            result
                ..add(r'id')
                ..add(serializers.serialize(object.id,
                    specifiedType: const FullType(num)));
        }
        if (object.deprecatedRef != null) {
            result
                ..add(r'deprecatedRef')
                ..add(serializers.serialize(object.deprecatedRef,
                    specifiedType: const FullType(DeprecatedObject)));
        }
        if (object.bars != null) {
            result
                ..add(r'bars')
                ..add(serializers.serialize(object.bars,
                    specifiedType: const FullType(BuiltList, [FullType(String)])));
        }
        return result;
    }

    @override
    ObjectWithDeprecatedFields deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = ObjectWithDeprecatedFieldsBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'uuid':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.uuid = valueDes;
                    break;
                case r'id':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(num)) as num;
                    result.id = valueDes;
                    break;
                case r'deprecatedRef':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(DeprecatedObject)) as DeprecatedObject;
                    result.deprecatedRef.replace(valueDes);
                    break;
                case r'bars':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(String)])) as BuiltList<String>;
                    result.bars.replace(valueDes);
                    break;
            }
        }
        return result.build();
    }
}


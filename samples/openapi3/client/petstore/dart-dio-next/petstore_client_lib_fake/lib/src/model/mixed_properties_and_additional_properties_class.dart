//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/animal.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'mixed_properties_and_additional_properties_class.g.dart';



abstract class MixedPropertiesAndAdditionalPropertiesClass implements Built<MixedPropertiesAndAdditionalPropertiesClass, MixedPropertiesAndAdditionalPropertiesClassBuilder> {
    @BuiltValueField(wireName: r'uuid')
    String? get uuid;

    @BuiltValueField(wireName: r'dateTime')
    DateTime? get dateTime;

    @BuiltValueField(wireName: r'map')
    BuiltMap<String, Animal>? get map;

    MixedPropertiesAndAdditionalPropertiesClass._();

    static void _initializeBuilder(MixedPropertiesAndAdditionalPropertiesClassBuilder b) => b;

    factory MixedPropertiesAndAdditionalPropertiesClass([void updates(MixedPropertiesAndAdditionalPropertiesClassBuilder b)]) = _$MixedPropertiesAndAdditionalPropertiesClass;

    @BuiltValueSerializer(custom: true)
    static Serializer<MixedPropertiesAndAdditionalPropertiesClass> get serializer => _$MixedPropertiesAndAdditionalPropertiesClassSerializer();
}

class _$MixedPropertiesAndAdditionalPropertiesClassSerializer implements StructuredSerializer<MixedPropertiesAndAdditionalPropertiesClass> {
    @override
    final Iterable<Type> types = const [MixedPropertiesAndAdditionalPropertiesClass, _$MixedPropertiesAndAdditionalPropertiesClass];

    @override
    final String wireName = r'MixedPropertiesAndAdditionalPropertiesClass';

    @override
    Iterable<Object?> serialize(Serializers serializers, MixedPropertiesAndAdditionalPropertiesClass object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.uuid != null) {
            result
                ..add(r'uuid')
                ..add(serializers.serialize(object.uuid,
                    specifiedType: const FullType(String)));
        }
        if (object.dateTime != null) {
            result
                ..add(r'dateTime')
                ..add(serializers.serialize(object.dateTime,
                    specifiedType: const FullType(DateTime)));
        }
        if (object.map != null) {
            result
                ..add(r'map')
                ..add(serializers.serialize(object.map,
                    specifiedType: const FullType(BuiltMap, [FullType(String), FullType(Animal)])));
        }
        return result;
    }

    @override
    MixedPropertiesAndAdditionalPropertiesClass deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = MixedPropertiesAndAdditionalPropertiesClassBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            switch (key) {
                case r'uuid':
                    result.uuid = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
                case r'dateTime':
                    result.dateTime = serializers.deserialize(value,
                        specifiedType: const FullType(DateTime)) as DateTime;
                    break;
                case r'map':
                    result.map.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(Animal)])) as BuiltMap<String, Animal>);
                    break;
            }
        }
        return result.build();
    }
}


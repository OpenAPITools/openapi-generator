//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'additional_properties_class.g.dart';

abstract class AdditionalPropertiesClass implements Built<AdditionalPropertiesClass, AdditionalPropertiesClassBuilder> {

    @nullable
    @BuiltValueField(wireName: r'map_property')
    BuiltMap<String, String> get mapProperty;

    @nullable
    @BuiltValueField(wireName: r'map_of_map_property')
    BuiltMap<String, BuiltMap<String, String>> get mapOfMapProperty;

    AdditionalPropertiesClass._();

    static void _initializeBuilder(AdditionalPropertiesClassBuilder b) => b;

    factory AdditionalPropertiesClass([void updates(AdditionalPropertiesClassBuilder b)]) = _$AdditionalPropertiesClass;

    @BuiltValueSerializer(custom: true)
    static Serializer<AdditionalPropertiesClass> get serializer => _$AdditionalPropertiesClassSerializer();
}

class _$AdditionalPropertiesClassSerializer implements StructuredSerializer<AdditionalPropertiesClass> {

    @override
    final Iterable<Type> types = const [AdditionalPropertiesClass, _$AdditionalPropertiesClass];
    @override
    final String wireName = r'AdditionalPropertiesClass';

    @override
    Iterable<Object> serialize(Serializers serializers, AdditionalPropertiesClass object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.mapProperty != null) {
            result
                ..add(r'map_property')
                ..add(serializers.serialize(object.mapProperty,
                    specifiedType: const FullType(BuiltMap, [FullType(String), FullType(String)])));
        }
        if (object.mapOfMapProperty != null) {
            result
                ..add(r'map_of_map_property')
                ..add(serializers.serialize(object.mapOfMapProperty,
                    specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(String)])])));
        }
        return result;
    }

    @override
    AdditionalPropertiesClass deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = AdditionalPropertiesClassBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'map_property':
                    result.mapProperty.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(String)])) as BuiltMap<String, String>);
                    break;
                case r'map_of_map_property':
                    result.mapOfMapProperty.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(String)])])) as BuiltMap<String, BuiltMap<String, String>>);
                    break;
            }
        }
        return result.build();
    }
}


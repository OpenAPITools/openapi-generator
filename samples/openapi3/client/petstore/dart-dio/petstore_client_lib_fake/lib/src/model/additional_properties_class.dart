//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'additional_properties_class.g.dart';

/// AdditionalPropertiesClass
///
/// Properties:
/// * [mapProperty] 
/// * [mapOfMapProperty] 
abstract class AdditionalPropertiesClass implements Built<AdditionalPropertiesClass, AdditionalPropertiesClassBuilder> {
    @BuiltValueField(wireName: r'map_property')
    BuiltMap<String, String>? get mapProperty;

    @BuiltValueField(wireName: r'map_of_map_property')
    BuiltMap<String, BuiltMap<String, String>>? get mapOfMapProperty;

    AdditionalPropertiesClass._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(AdditionalPropertiesClassBuilder b) => b;

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
    Iterable<Object?> serialize(Serializers serializers, AdditionalPropertiesClass object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
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
    AdditionalPropertiesClass deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = AdditionalPropertiesClassBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'map_property':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(String)])) as BuiltMap<String, String>;
                    result.mapProperty.replace(valueDes);
                    break;
                case r'map_of_map_property':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(String)])])) as BuiltMap<String, BuiltMap<String, String>>;
                    result.mapOfMapProperty.replace(valueDes);
                    break;
            }
        }
        return result.build();
    }
}


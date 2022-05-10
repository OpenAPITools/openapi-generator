//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';
import 'package:one_of/any_of.dart';
// ignore_for_file: unused_element, unused_import

part 'dog_all_of.g.dart';

/// DogAllOf
///
/// Properties:
/// * [breed] 
@BuiltValue(instantiable: false)
abstract class DogAllOf  {
    @BuiltValueField(wireName: r'breed')
    String? get breed;



    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(DogAllOfBuilder b) => b;

    @BuiltValueSerializer(custom: true)
    static Serializer<DogAllOf> get serializer => _$DogAllOfSerializer();


}

class _$DogAllOfSerializer implements PrimitiveSerializer<DogAllOf> {
    @override
    final Iterable<Type> types = const [DogAllOf];

    @override
    final String wireName = r'DogAllOf';

    Iterable<Object?> _serializeProperties(Serializers serializers, DogAllOf object,
        {FullType specifiedType = FullType.unspecified}) sync* {        
        if (object.breed != null) {
            yield r'breed';
            yield serializers.serialize(object.breed,
                    specifiedType: const FullType(String));
        }
    }

    @override
    Object serialize(Serializers serializers, DogAllOf object,
        {FullType specifiedType = FullType.unspecified}) {
        return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
    }

    void _deserializeProperties(Serializers serializers, Object serialized,
        {FullType specifiedType = FullType.unspecified, required List<Object?> serializedList,required DogAllOfBuilder result, required List<Object?> unhandled}) {
        for (var i = 0; i < serializedList.length; i += 2) {
            final key = serializedList[i] as String;
            final value = serializedList[i + 1];
            switch (key) {
                 case r'breed':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.breed = valueDes;
                    break;
                default:
                  unhandled.add(key);
                  unhandled.add(value);
                  break;
            }
        }
    }
    
    @override
    DogAllOf deserialize(Serializers serializers, Object serialized,
        {FullType specifiedType = FullType.unspecified}) {
        return serializers.deserialize(serialized, specifiedType: FullType($DogAllOf)) as $DogAllOf;
    }
}

/// a concrete implmentation of [DogAllOf], since [DogAllOf] is not instantiable
@BuiltValue(instantiable: true)
abstract class $DogAllOf implements DogAllOf, Built<$DogAllOf, $DogAllOfBuilder> {
  $DogAllOf._();

  factory $DogAllOf([void Function($DogAllOfBuilder)? updates]) = _$$DogAllOf;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults($DogAllOfBuilder b) => b;
  @BuiltValueSerializer(custom: true)
  static Serializer<$DogAllOf> get serializer => _$$DogAllOfSerializer();
}

class _$$DogAllOfSerializer implements PrimitiveSerializer<$DogAllOf> {
    @override
    final Iterable<Type> types = const [$DogAllOf, _$$DogAllOf];

    @override
    final String wireName = r'$DogAllOf';



    @override
    Object serialize(Serializers serializers, $DogAllOf object,
        {FullType specifiedType = FullType.unspecified}) {        
        return serializers.serialize(object, specifiedType: FullType(DogAllOf))!;
    }

    void _deserializeProperties(Serializers serializers, Object serialized,
        {FullType specifiedType = FullType.unspecified, required List<Object?> serializedList,required $DogAllOfBuilder result, required List<Object?> unhandled}) {
        for (var i = 0; i < serializedList.length; i += 2) {
            final key = serializedList[i] as String;
            final value = serializedList[i + 1];
            switch (key) {
                 case r'breed':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.breed = valueDes;
                    break;
                default:
                  unhandled.add(key);
                  unhandled.add(value);
                  break;
            }
        }
    }
    @override
    $DogAllOf deserialize(Serializers serializers, Object serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = $DogAllOfBuilder();
        final serializedList = (serialized as Iterable<Object?>).toList();
        final unhandled = <Object?>[];
        _deserializeProperties(serializers, serialized, specifiedType: specifiedType, serializedList: serializedList, unhandled: unhandled, result: result);
        return result.build();        
    }
}




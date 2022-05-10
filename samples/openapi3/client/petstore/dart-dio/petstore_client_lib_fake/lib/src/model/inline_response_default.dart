//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:openapi/src/model/foo.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';
import 'package:one_of/any_of.dart';
// ignore_for_file: unused_element, unused_import

part 'inline_response_default.g.dart';

/// InlineResponseDefault
///
/// Properties:
/// * [string] 
@BuiltValue()
abstract class InlineResponseDefault implements Built<InlineResponseDefault, InlineResponseDefaultBuilder> {
    @BuiltValueField(wireName: r'string')
    Foo? get string;


    InlineResponseDefault._();
    
    factory InlineResponseDefault([void updates(InlineResponseDefaultBuilder b)]) = _$InlineResponseDefault;

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(InlineResponseDefaultBuilder b) => b;

    @BuiltValueSerializer(custom: true)
    static Serializer<InlineResponseDefault> get serializer => _$InlineResponseDefaultSerializer();


}

class _$InlineResponseDefaultSerializer implements PrimitiveSerializer<InlineResponseDefault> {
    @override
    final Iterable<Type> types = const [InlineResponseDefault, _$InlineResponseDefault];

    @override
    final String wireName = r'InlineResponseDefault';

    Iterable<Object?> _serializeProperties(Serializers serializers, InlineResponseDefault object,
        {FullType specifiedType = FullType.unspecified}) sync* {        
        if (object.string != null) {
            yield r'string';
            yield serializers.serialize(object.string,
                    specifiedType: const FullType(Foo));
        }
    }

    @override
    Object serialize(Serializers serializers, InlineResponseDefault object,
        {FullType specifiedType = FullType.unspecified}) {
        return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
    }

    void _deserializeProperties(Serializers serializers, Object serialized,
        {FullType specifiedType = FullType.unspecified, required List<Object?> serializedList,required InlineResponseDefaultBuilder result, required List<Object?> unhandled}) {
        for (var i = 0; i < serializedList.length; i += 2) {
            final key = serializedList[i] as String;
            final value = serializedList[i + 1];
            switch (key) {
                 case r'string':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(Foo)) as Foo;
                    result.string.replace(valueDes);
                    break;
                default:
                  unhandled.add(key);
                  unhandled.add(value);
                  break;
            }
        }
    }
    
    @override
    InlineResponseDefault deserialize(Serializers serializers, Object serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = InlineResponseDefaultBuilder();
        final serializedList = (serialized as Iterable<Object?>).toList();        
        final unhandled = <Object?>[];
        _deserializeProperties(serializers, serialized, specifiedType: specifiedType, serializedList: serializedList, unhandled: unhandled, result: result);        
        return result.build();
    }
}





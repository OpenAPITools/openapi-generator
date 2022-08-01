//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'special_model_name.g.dart';

/// SpecialModelName
///
/// Properties:
/// * [dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket] 
abstract class SpecialModelName implements Built<SpecialModelName, SpecialModelNameBuilder> {
    @BuiltValueField(wireName: r'$special[property.name]')
    int? get dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket;

    SpecialModelName._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(SpecialModelNameBuilder b) => b;

    factory SpecialModelName([void updates(SpecialModelNameBuilder b)]) = _$SpecialModelName;

    @BuiltValueSerializer(custom: true)
    static Serializer<SpecialModelName> get serializer => _$SpecialModelNameSerializer();
}

class _$SpecialModelNameSerializer implements StructuredSerializer<SpecialModelName> {
    @override
    final Iterable<Type> types = const [SpecialModelName, _$SpecialModelName];

    @override
    final String wireName = r'SpecialModelName';

    @override
    Iterable<Object?> serialize(Serializers serializers, SpecialModelName object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket != null) {
            result
                ..add(r'$special[property.name]')
                ..add(serializers.serialize(object.dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket,
                    specifiedType: const FullType(int)));
        }
        return result;
    }

    @override
    SpecialModelName deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = SpecialModelNameBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'$special[property.name]':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    result.dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket = valueDes;
                    break;
            }
        }
        return result.build();
    }
}


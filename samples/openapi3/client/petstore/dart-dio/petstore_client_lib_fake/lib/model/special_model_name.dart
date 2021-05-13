//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'special_model_name.g.dart';

abstract class SpecialModelName implements Built<SpecialModelName, SpecialModelNameBuilder> {

    @nullable
    @BuiltValueField(wireName: r'$special[property.name]')
    int get dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket;

    SpecialModelName._();

    static void _initializeBuilder(SpecialModelNameBuilder b) => b;

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
    Iterable<Object> serialize(Serializers serializers, SpecialModelName object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket != null) {
            result
                ..add(r'$special[property.name]')
                ..add(serializers.serialize(object.dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket,
                    specifiedType: const FullType(int)));
        }
        return result;
    }

    @override
    SpecialModelName deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = SpecialModelNameBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'$special[property.name]':
                    result.dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
            }
        }
        return result.build();
    }
}


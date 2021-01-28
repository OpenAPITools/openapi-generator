//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'special_model_name.g.dart';

abstract class SpecialModelName implements Built<SpecialModelName, SpecialModelNameBuilder> {

    @nullable
    @BuiltValueField(wireName: r'$special[property.name]')
    int get dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket;

    // Boilerplate code needed to wire-up generated code
    SpecialModelName._();

    static void _initializeBuilder(SpecialModelNameBuilder b) => b;

    factory SpecialModelName([void updates(SpecialModelNameBuilder b)]) = _$SpecialModelName;
    static Serializer<SpecialModelName> get serializer => _$specialModelNameSerializer;
}


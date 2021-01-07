//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'capitalization.g.dart';

abstract class Capitalization implements Built<Capitalization, CapitalizationBuilder> {

    @nullable
    @BuiltValueField(wireName: r'smallCamel')
    String get smallCamel;

    @nullable
    @BuiltValueField(wireName: r'CapitalCamel')
    String get capitalCamel;

    @nullable
    @BuiltValueField(wireName: r'small_Snake')
    String get smallSnake;

    @nullable
    @BuiltValueField(wireName: r'Capital_Snake')
    String get capitalSnake;

    @nullable
    @BuiltValueField(wireName: r'SCA_ETH_Flow_Points')
    String get sCAETHFlowPoints;

    /// Name of the pet 
    @nullable
    @BuiltValueField(wireName: r'ATT_NAME')
    String get ATT_NAME;

    // Boilerplate code needed to wire-up generated code
    Capitalization._();

    static void _initializeBuilder(CapitalizationBuilder b) => b;

    factory Capitalization([void updates(CapitalizationBuilder b)]) = _$Capitalization;
    static Serializer<Capitalization> get serializer => _$capitalizationSerializer;
}


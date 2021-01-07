//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'outer_composite.g.dart';

abstract class OuterComposite implements Built<OuterComposite, OuterCompositeBuilder> {

    @nullable
    @BuiltValueField(wireName: r'my_number')
    num get myNumber;

    @nullable
    @BuiltValueField(wireName: r'my_string')
    String get myString;

    @nullable
    @BuiltValueField(wireName: r'my_boolean')
    bool get myBoolean;

    // Boilerplate code needed to wire-up generated code
    OuterComposite._();

    static void _initializeBuilder(OuterCompositeBuilder b) => b;

    factory OuterComposite([void updates(OuterCompositeBuilder b)]) = _$OuterComposite;
    static Serializer<OuterComposite> get serializer => _$outerCompositeSerializer;
}


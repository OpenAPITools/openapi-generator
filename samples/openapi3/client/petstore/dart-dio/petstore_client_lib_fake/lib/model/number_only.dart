//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'number_only.g.dart';

abstract class NumberOnly implements Built<NumberOnly, NumberOnlyBuilder> {

    @nullable
    @BuiltValueField(wireName: r'JustNumber')
    num get justNumber;

    // Boilerplate code needed to wire-up generated code
    NumberOnly._();

    static void _initializeBuilder(NumberOnlyBuilder b) => b;

    factory NumberOnly([void updates(NumberOnlyBuilder b)]) = _$NumberOnly;
    static Serializer<NumberOnly> get serializer => _$numberOnlySerializer;
}


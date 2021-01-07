//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'array_of_number_only.g.dart';

abstract class ArrayOfNumberOnly implements Built<ArrayOfNumberOnly, ArrayOfNumberOnlyBuilder> {

    @nullable
    @BuiltValueField(wireName: r'ArrayNumber')
    BuiltList<num> get arrayNumber;

    // Boilerplate code needed to wire-up generated code
    ArrayOfNumberOnly._();

    static void _initializeBuilder(ArrayOfNumberOnlyBuilder b) => b;

    factory ArrayOfNumberOnly([void updates(ArrayOfNumberOnlyBuilder b)]) = _$ArrayOfNumberOnly;
    static Serializer<ArrayOfNumberOnly> get serializer => _$arrayOfNumberOnlySerializer;
}


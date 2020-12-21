//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:openapi/model/read_only_first.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'array_test.g.dart';

abstract class ArrayTest implements Built<ArrayTest, ArrayTestBuilder> {

    @nullable
    @BuiltValueField(wireName: r'array_of_string')
    BuiltList<String> get arrayOfString;

    @nullable
    @BuiltValueField(wireName: r'array_array_of_integer')
    BuiltList<BuiltList<int>> get arrayArrayOfInteger;

    @nullable
    @BuiltValueField(wireName: r'array_array_of_model')
    BuiltList<BuiltList<ReadOnlyFirst>> get arrayArrayOfModel;

    // Boilerplate code needed to wire-up generated code
    ArrayTest._();

    static void _initializeBuilder(ArrayTestBuilder b) => b;

    factory ArrayTest([void updates(ArrayTestBuilder b)]) = _$ArrayTest;
    static Serializer<ArrayTest> get serializer => _$arrayTestSerializer;
}


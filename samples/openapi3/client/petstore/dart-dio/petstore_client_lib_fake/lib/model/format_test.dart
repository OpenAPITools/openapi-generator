//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'dart:typed_data';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'format_test.g.dart';

abstract class FormatTest implements Built<FormatTest, FormatTestBuilder> {

    @BuiltValueField(wireName: r'integer')
    int get integer;

    @BuiltValueField(wireName: r'int32')
    int get int32;

    @BuiltValueField(wireName: r'int64')
    int get int64;

    @BuiltValueField(wireName: r'number')
    num get number;

    @BuiltValueField(wireName: r'float')
    double get float;

    @BuiltValueField(wireName: r'double')
    double get double_;

    @BuiltValueField(wireName: r'decimal')
    double get decimal;

    @BuiltValueField(wireName: r'string')
    String get string;

    @BuiltValueField(wireName: r'byte')
    String get byte;

    @BuiltValueField(wireName: r'binary')
    Uint8List get binary;

    @BuiltValueField(wireName: r'date')
    DateTime get date;

    @BuiltValueField(wireName: r'dateTime')
    DateTime get dateTime;

    @BuiltValueField(wireName: r'uuid')
    String get uuid;

    @BuiltValueField(wireName: r'password')
    String get password;

    /// A string that is a 10 digit number. Can have leading zeros.
    @BuiltValueField(wireName: r'pattern_with_digits')
    String get patternWithDigits;

    /// A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
    @BuiltValueField(wireName: r'pattern_with_digits_and_delimiter')
    String get patternWithDigitsAndDelimiter;

    // Boilerplate code needed to wire-up generated code
    FormatTest._();

    static void _initializeBuilder(FormatTestBuilder b) => b;

    factory FormatTest([void updates(FormatTestBuilder b)]) = _$FormatTest;
    static Serializer<FormatTest> get serializer => _$formatTestSerializer;
}


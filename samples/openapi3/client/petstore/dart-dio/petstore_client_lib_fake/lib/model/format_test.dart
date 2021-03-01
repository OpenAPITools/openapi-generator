//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'dart:typed_data';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'format_test.g.dart';

abstract class FormatTest implements Built<FormatTest, FormatTestBuilder> {

    @nullable
    @BuiltValueField(wireName: r'integer')
    int get integer;

    @nullable
    @BuiltValueField(wireName: r'int32')
    int get int32;

    @nullable
    @BuiltValueField(wireName: r'int64')
    int get int64;

    @BuiltValueField(wireName: r'number')
    num get number;

    @nullable
    @BuiltValueField(wireName: r'float')
    double get float;

    @nullable
    @BuiltValueField(wireName: r'double')
    double get double_;

    @nullable
    @BuiltValueField(wireName: r'decimal')
    double get decimal;

    @nullable
    @BuiltValueField(wireName: r'string')
    String get string;

    @BuiltValueField(wireName: r'byte')
    String get byte;

    @nullable
    @BuiltValueField(wireName: r'binary')
    Uint8List get binary;

    @BuiltValueField(wireName: r'date')
    DateTime get date;

    @nullable
    @BuiltValueField(wireName: r'dateTime')
    DateTime get dateTime;

    @nullable
    @BuiltValueField(wireName: r'uuid')
    String get uuid;

    @BuiltValueField(wireName: r'password')
    String get password;

    /// A string that is a 10 digit number. Can have leading zeros.
    @nullable
    @BuiltValueField(wireName: r'pattern_with_digits')
    String get patternWithDigits;

    /// A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
    @nullable
    @BuiltValueField(wireName: r'pattern_with_digits_and_delimiter')
    String get patternWithDigitsAndDelimiter;

    FormatTest._();

    static void _initializeBuilder(FormatTestBuilder b) => b;

    factory FormatTest([void updates(FormatTestBuilder b)]) = _$FormatTest;

    @BuiltValueSerializer(custom: true)
    static Serializer<FormatTest> get serializer => _$FormatTestSerializer();
}

class _$FormatTestSerializer implements StructuredSerializer<FormatTest> {

    @override
    final Iterable<Type> types = const [FormatTest, _$FormatTest];
    @override
    final String wireName = r'FormatTest';

    @override
    Iterable<Object> serialize(Serializers serializers, FormatTest object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.integer != null) {
            result
                ..add(r'integer')
                ..add(serializers.serialize(object.integer,
                    specifiedType: const FullType(int)));
        }
        if (object.int32 != null) {
            result
                ..add(r'int32')
                ..add(serializers.serialize(object.int32,
                    specifiedType: const FullType(int)));
        }
        if (object.int64 != null) {
            result
                ..add(r'int64')
                ..add(serializers.serialize(object.int64,
                    specifiedType: const FullType(int)));
        }
        result
            ..add(r'number')
            ..add(serializers.serialize(object.number,
                specifiedType: const FullType(num)));
        if (object.float != null) {
            result
                ..add(r'float')
                ..add(serializers.serialize(object.float,
                    specifiedType: const FullType(double)));
        }
        if (object.double_ != null) {
            result
                ..add(r'double')
                ..add(serializers.serialize(object.double_,
                    specifiedType: const FullType(double)));
        }
        if (object.decimal != null) {
            result
                ..add(r'decimal')
                ..add(serializers.serialize(object.decimal,
                    specifiedType: const FullType(double)));
        }
        if (object.string != null) {
            result
                ..add(r'string')
                ..add(serializers.serialize(object.string,
                    specifiedType: const FullType(String)));
        }
        result
            ..add(r'byte')
            ..add(serializers.serialize(object.byte,
                specifiedType: const FullType(String)));
        if (object.binary != null) {
            result
                ..add(r'binary')
                ..add(serializers.serialize(object.binary,
                    specifiedType: const FullType(Uint8List)));
        }
        result
            ..add(r'date')
            ..add(serializers.serialize(object.date,
                specifiedType: const FullType(DateTime)));
        if (object.dateTime != null) {
            result
                ..add(r'dateTime')
                ..add(serializers.serialize(object.dateTime,
                    specifiedType: const FullType(DateTime)));
        }
        if (object.uuid != null) {
            result
                ..add(r'uuid')
                ..add(serializers.serialize(object.uuid,
                    specifiedType: const FullType(String)));
        }
        result
            ..add(r'password')
            ..add(serializers.serialize(object.password,
                specifiedType: const FullType(String)));
        if (object.patternWithDigits != null) {
            result
                ..add(r'pattern_with_digits')
                ..add(serializers.serialize(object.patternWithDigits,
                    specifiedType: const FullType(String)));
        }
        if (object.patternWithDigitsAndDelimiter != null) {
            result
                ..add(r'pattern_with_digits_and_delimiter')
                ..add(serializers.serialize(object.patternWithDigitsAndDelimiter,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    FormatTest deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = FormatTestBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'integer':
                    result.integer = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
                case r'int32':
                    result.int32 = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
                case r'int64':
                    result.int64 = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
                case r'number':
                    result.number = serializers.deserialize(value,
                        specifiedType: const FullType(num)) as num;
                    break;
                case r'float':
                    result.float = serializers.deserialize(value,
                        specifiedType: const FullType(double)) as double;
                    break;
                case r'double':
                    result.double_ = serializers.deserialize(value,
                        specifiedType: const FullType(double)) as double;
                    break;
                case r'decimal':
                    result.decimal = serializers.deserialize(value,
                        specifiedType: const FullType(double)) as double;
                    break;
                case r'string':
                    result.string = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
                case r'byte':
                    result.byte = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
                case r'binary':
                    result.binary = serializers.deserialize(value,
                        specifiedType: const FullType(Uint8List)) as Uint8List;
                    break;
                case r'date':
                    result.date = serializers.deserialize(value,
                        specifiedType: const FullType(DateTime)) as DateTime;
                    break;
                case r'dateTime':
                    result.dateTime = serializers.deserialize(value,
                        specifiedType: const FullType(DateTime)) as DateTime;
                    break;
                case r'uuid':
                    result.uuid = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
                case r'password':
                    result.password = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
                case r'pattern_with_digits':
                    result.patternWithDigits = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
                case r'pattern_with_digits_and_delimiter':
                    result.patternWithDigitsAndDelimiter = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
            }
        }
        return result.build();
    }
}


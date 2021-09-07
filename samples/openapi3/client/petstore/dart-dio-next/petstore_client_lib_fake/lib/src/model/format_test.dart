//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'dart:typed_data';
import 'package:openapi/src/model/date.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'format_test.g.dart';

/// FormatTest
///
/// Properties:
/// * [integer] 
/// * [int32] 
/// * [int64] 
/// * [number] 
/// * [float] 
/// * [double_] 
/// * [decimal] 
/// * [string] 
/// * [byte] 
/// * [binary] 
/// * [date] 
/// * [dateTime] 
/// * [uuid] 
/// * [password] 
/// * [patternWithDigits] - A string that is a 10 digit number. Can have leading zeros.
/// * [patternWithDigitsAndDelimiter] - A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
abstract class FormatTest implements Built<FormatTest, FormatTestBuilder> {
    @BuiltValueField(wireName: r'integer')
    int? get integer;

    @BuiltValueField(wireName: r'int32')
    int? get int32;

    @BuiltValueField(wireName: r'int64')
    int? get int64;

    @BuiltValueField(wireName: r'number')
    num get number;

    @BuiltValueField(wireName: r'float')
    double? get float;

    @BuiltValueField(wireName: r'double')
    double? get double_;

    @BuiltValueField(wireName: r'decimal')
    double? get decimal;

    @BuiltValueField(wireName: r'string')
    String? get string;

    @BuiltValueField(wireName: r'byte')
    String get byte;

    @BuiltValueField(wireName: r'binary')
    Uint8List? get binary;

    @BuiltValueField(wireName: r'date')
    Date get date;

    @BuiltValueField(wireName: r'dateTime')
    DateTime? get dateTime;

    @BuiltValueField(wireName: r'uuid')
    String? get uuid;

    @BuiltValueField(wireName: r'password')
    String get password;

    /// A string that is a 10 digit number. Can have leading zeros.
    @BuiltValueField(wireName: r'pattern_with_digits')
    String? get patternWithDigits;

    /// A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
    @BuiltValueField(wireName: r'pattern_with_digits_and_delimiter')
    String? get patternWithDigitsAndDelimiter;

    FormatTest._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(FormatTestBuilder b) => b;

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
    Iterable<Object?> serialize(Serializers serializers, FormatTest object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
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
                specifiedType: const FullType(Date)));
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
    FormatTest deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = FormatTestBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'integer':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    result.integer = valueDes;
                    break;
                case r'int32':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    result.int32 = valueDes;
                    break;
                case r'int64':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    result.int64 = valueDes;
                    break;
                case r'number':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(num)) as num;
                    result.number = valueDes;
                    break;
                case r'float':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(double)) as double;
                    result.float = valueDes;
                    break;
                case r'double':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(double)) as double;
                    result.double_ = valueDes;
                    break;
                case r'decimal':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(double)) as double;
                    result.decimal = valueDes;
                    break;
                case r'string':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.string = valueDes;
                    break;
                case r'byte':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.byte = valueDes;
                    break;
                case r'binary':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(Uint8List)) as Uint8List;
                    result.binary = valueDes;
                    break;
                case r'date':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(Date)) as Date;
                    result.date = valueDes;
                    break;
                case r'dateTime':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(DateTime)) as DateTime;
                    result.dateTime = valueDes;
                    break;
                case r'uuid':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.uuid = valueDes;
                    break;
                case r'password':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.password = valueDes;
                    break;
                case r'pattern_with_digits':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.patternWithDigits = valueDes;
                    break;
                case r'pattern_with_digits_and_delimiter':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.patternWithDigitsAndDelimiter = valueDes;
                    break;
            }
        }
        return result.build();
    }
}


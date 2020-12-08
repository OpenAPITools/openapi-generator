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
    
    @nullable
    @BuiltValueField(wireName: r'number')
    num get number;
    
    @nullable
    @BuiltValueField(wireName: r'float')
    double get float;
    
    @nullable
    @BuiltValueField(wireName: r'double')
    double get double;
    
    @nullable
    @BuiltValueField(wireName: r'decimal')
    double get decimal;
    
    @nullable
    @BuiltValueField(wireName: r'string')
    String get string;
    
    @nullable
    @BuiltValueField(wireName: r'byte')
    String get byte;
    
    @nullable
    @BuiltValueField(wireName: r'binary')
    Uint8List get binary;
    
    @nullable
    @BuiltValueField(wireName: r'date')
    DateTime get date;
    
    @nullable
    @BuiltValueField(wireName: r'dateTime')
    DateTime get dateTime;
    
    @nullable
    @BuiltValueField(wireName: r'uuid')
    String get uuid;
    
    @nullable
    @BuiltValueField(wireName: r'password')
    String get password;
    /* A string that is a 10 digit number. Can have leading zeros. */
    @nullable
    @BuiltValueField(wireName: r'pattern_with_digits')
    String get patternWithDigits;
    /* A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01. */
    @nullable
    @BuiltValueField(wireName: r'pattern_with_digits_and_delimiter')
    String get patternWithDigitsAndDelimiter;

    // Boilerplate code needed to wire-up generated code
    FormatTest._();

    factory FormatTest([updates(FormatTestBuilder b)]) = _$FormatTest;
    static Serializer<FormatTest> get serializer => _$formatTestSerializer;
}


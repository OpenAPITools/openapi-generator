// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'format_test.reflection.dart';
part 'format_test.serialization.dart';


/// FormatTestMixin
///
/// Properties:
/// * [integer] 
/// * [int32] 
/// * [int64] 
/// * [number] 
/// * [float] 
/// * [$double] 
/// * [decimal] 
/// * [string] 
/// * [byte] 
/// * [binary] 
/// * [date] 
/// * [dateTime] 
/// * [uuid] 
/// * [uuidWithDefault] 
/// * [password] 
/// * [patternWithDigits] - A string that is a 10 digit number. Can have leading zeros.
/// * [patternWithDigitsAndDelimiter] - A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
mixin FormatTestMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<int> get integer;
  UndefinedWrapper<int> get int32;
  UndefinedWrapper<int> get int64;
  num get number;
  UndefinedWrapper<double> get float;
  UndefinedWrapper<double> get $double;
  UndefinedWrapper<double> get decimal;
  UndefinedWrapper<String> get string;
  Uint8List get byte;
  UndefinedWrapper<MyFile> get binary;
  DateTime get date;
  UndefinedWrapper<DateTime> get dateTime;
  UndefinedWrapper<String> get uuid;
  UndefinedWrapper<String> get uuidWithDefault;
  String get password;
  UndefinedWrapper<String> get patternWithDigits;
  UndefinedWrapper<String> get patternWithDigitsAndDelimiter;

}

/// FormatTest
///
/// Properties:
/// * [integer] 
/// * [int32] 
/// * [int64] 
/// * [number] 
/// * [float] 
/// * [$double] 
/// * [decimal] 
/// * [string] 
/// * [byte] 
/// * [binary] 
/// * [date] 
/// * [dateTime] 
/// * [uuid] 
/// * [uuidWithDefault] 
/// * [password] 
/// * [patternWithDigits] - A string that is a 10 digit number. Can have leading zeros.
/// * [patternWithDigitsAndDelimiter] - A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
class FormatTest with
$OpenApiObjectMixin,


FormatTestMixin {
  @override
  UndefinedWrapper<int> integer;
  @override
  UndefinedWrapper<int> int32;
  @override
  UndefinedWrapper<int> int64;
  @override
  num number;
  @override
  UndefinedWrapper<double> float;
  @override
  UndefinedWrapper<double> $double;
  @override
  UndefinedWrapper<double> decimal;
  @override
  UndefinedWrapper<String> string;
  @override
  Uint8List byte;
  @override
  UndefinedWrapper<MyFile> binary;
  @override
  DateTime date;
  @override
  UndefinedWrapper<DateTime> dateTime;
  @override
  UndefinedWrapper<String> uuid;
  @override
  UndefinedWrapper<String> uuidWithDefault;
  @override
  String password;
  @override
  UndefinedWrapper<String> patternWithDigits;
  @override
  UndefinedWrapper<String> patternWithDigitsAndDelimiter;





  FormatTest.$all({
    required this.integer,
    required this.int32,
    required this.int64,
    required this.number,
    required this.float,
    required this.$double,
    required this.decimal,
    required this.string,
    required this.byte,
    required this.binary,
    required this.date,
    required this.dateTime,
    required this.uuid,
    required this.uuidWithDefault,
    required this.password,
    required this.patternWithDigits,
    required this.patternWithDigitsAndDelimiter,
    
    
  });

  FormatTest({
    this.integer = const UndefinedWrapper.undefined(),
    this.int32 = const UndefinedWrapper.undefined(),
    this.int64 = const UndefinedWrapper.undefined(),
  required  this.number ,
    this.float = const UndefinedWrapper.undefined(),
    this.$double = const UndefinedWrapper.undefined(),
    this.decimal = const UndefinedWrapper.undefined(),
    this.string = const UndefinedWrapper.undefined(),
  required  this.byte ,
    this.binary = const UndefinedWrapper.undefined(),
  required  this.date ,
    this.dateTime = const UndefinedWrapper.undefined(),
    this.uuid = const UndefinedWrapper.undefined(),
    this.uuidWithDefault = const UndefinedWrapper('11111111-206d-4f12-9f12-3d1e525a8e84'),
  required  this.password ,
    this.patternWithDigits = const UndefinedWrapper.undefined(),
    this.patternWithDigitsAndDelimiter = const UndefinedWrapper.undefined(),
    
    
  });
}





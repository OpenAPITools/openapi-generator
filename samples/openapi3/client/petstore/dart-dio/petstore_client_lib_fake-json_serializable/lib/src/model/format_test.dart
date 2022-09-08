//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:dio/dio.dart';
import 'package:json_annotation/json_annotation.dart';

part 'format_test.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FormatTest {
  /// Returns a new [FormatTest] instance.
  FormatTest({

     this.integer,

     this.int32,

     this.int64,

    required  this.number,

     this.float,

     this.double_,

     this.decimal,

     this.string,

    required  this.byte,

     this.binary,

    required  this.date,

     this.dateTime,

     this.uuid,

    required  this.password,

     this.patternWithDigits,

     this.patternWithDigitsAndDelimiter,
  });

          // minimum: 10
          // maximum: 100
  @JsonKey(
    
    name: r'integer',
    required: false,
    includeIfNull: false
  )


  final int? integer;



          // minimum: 20
          // maximum: 200
  @JsonKey(
    
    name: r'int32',
    required: false,
    includeIfNull: false
  )


  final int? int32;



  @JsonKey(
    
    name: r'int64',
    required: false,
    includeIfNull: false
  )


  final int? int64;



          // minimum: 32.1
          // maximum: 543.2
  @JsonKey(
    
    name: r'number',
    required: true,
    includeIfNull: false
  )


  final num number;



          // minimum: 54.3
          // maximum: 987.6
  @JsonKey(
    
    name: r'float',
    required: false,
    includeIfNull: false
  )


  final double? float;



          // minimum: 67.8
          // maximum: 123.4
  @JsonKey(
    
    name: r'double',
    required: false,
    includeIfNull: false
  )


  final double? double_;



  @JsonKey(
    
    name: r'decimal',
    required: false,
    includeIfNull: false
  )


  final double? decimal;



  @JsonKey(
    
    name: r'string',
    required: false,
    includeIfNull: false
  )


  final String? string;



  @JsonKey(
    
    name: r'byte',
    required: true,
    includeIfNull: false
  )


  final String byte;



  @JsonKey(ignore: true)


  final MultipartFile? binary;



  @JsonKey(
    
    name: r'date',
    required: true,
    includeIfNull: false
  )


  final DateTime date;



  @JsonKey(
    
    name: r'dateTime',
    required: false,
    includeIfNull: false
  )


  final DateTime? dateTime;



  @JsonKey(
    
    name: r'uuid',
    required: false,
    includeIfNull: false
  )


  final String? uuid;



  @JsonKey(
    
    name: r'password',
    required: true,
    includeIfNull: false
  )


  final String password;



      /// A string that is a 10 digit number. Can have leading zeros.
  @JsonKey(
    
    name: r'pattern_with_digits',
    required: false,
    includeIfNull: false
  )


  final String? patternWithDigits;



      /// A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
  @JsonKey(
    
    name: r'pattern_with_digits_and_delimiter',
    required: false,
    includeIfNull: false
  )


  final String? patternWithDigitsAndDelimiter;



  @override
  bool operator ==(Object other) => identical(this, other) || other is FormatTest &&
     other.integer == integer &&
     other.int32 == int32 &&
     other.int64 == int64 &&
     other.number == number &&
     other.float == float &&
     other.double_ == double_ &&
     other.decimal == decimal &&
     other.string == string &&
     other.byte == byte &&
     other.binary == binary &&
     other.date == date &&
     other.dateTime == dateTime &&
     other.uuid == uuid &&
     other.password == password &&
     other.patternWithDigits == patternWithDigits &&
     other.patternWithDigitsAndDelimiter == patternWithDigitsAndDelimiter;

  @override
  int get hashCode =>
    integer.hashCode +
    int32.hashCode +
    int64.hashCode +
    number.hashCode +
    float.hashCode +
    double_.hashCode +
    decimal.hashCode +
    string.hashCode +
    byte.hashCode +
    binary.hashCode +
    date.hashCode +
    dateTime.hashCode +
    uuid.hashCode +
    password.hashCode +
    patternWithDigits.hashCode +
    patternWithDigitsAndDelimiter.hashCode;

  factory FormatTest.fromJson(Map<String, dynamic> json) => _$FormatTestFromJson(json);

  Map<String, dynamic> toJson() => _$FormatTestToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}


//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

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

        @freezed
        class FormatTest with _$FormatTest {
        const FormatTest._();
        
        const factory FormatTest({
                    @JsonKey(name: r'integer') 
    int?
 integer,
                    @JsonKey(name: r'int32') 
    int?
 int32,
                    @JsonKey(name: r'int64') 
    int?
 int64,
                    @JsonKey(name: r'number') 
    required num
 number,
                    @JsonKey(name: r'float') 
    double?
 float,
                    @JsonKey(name: r'double') 
    double?
 double_,
                    @JsonKey(name: r'decimal') 
    double?
 decimal,
                    @JsonKey(name: r'string') 
    String?
 string,
                    @JsonKey(name: r'byte') 
    required String
 byte,
                    @JsonKey(name: r'binary') 
    MultipartFile?
 binary,
                    @JsonKey(name: r'date') 
    required DateTime
 date,
                    @JsonKey(name: r'dateTime') 
    DateTime?
 dateTime,
                    @JsonKey(name: r'uuid') 
    String?
 uuid,
                    @JsonKey(name: r'password') 
    required String
 password,
                        /// A string that is a 10 digit number. Can have leading zeros.
            @JsonKey(name: r'pattern_with_digits') 
    String?
 patternWithDigits,
                        /// A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
            @JsonKey(name: r'pattern_with_digits_and_delimiter') 
    String?
 patternWithDigitsAndDelimiter,
        }) = _FormatTest;


        factory FormatTest.fromJson(Map<String, dynamic> json) => _$FormatTestFromJson(json);






}




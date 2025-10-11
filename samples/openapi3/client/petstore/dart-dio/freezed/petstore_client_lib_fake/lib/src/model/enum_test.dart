//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// EnumTest
    ///
    /// Properties:
        /// * [enumString] 
        /// * [enumStringRequired] 
        /// * [enumInteger] 
        /// * [enumNumber] 
        /// * [outerEnum] 
        /// * [outerEnumInteger] 
        /// * [outerEnumDefaultValue] 
        /// * [outerEnumIntegerDefaultValue] 

        @freezed
        class EnumTest with _$EnumTest {
        const EnumTest._();
        
        const factory EnumTest({
                    @JsonKey(name: r'enum_string') 
    EnumTestEnumStringEnum?
 enumString,
                    @JsonKey(name: r'enum_string_required') 
    required EnumTestEnumStringRequiredEnum
 enumStringRequired,
                    @JsonKey(name: r'enum_integer') 
    EnumTestEnumIntegerEnum?
 enumInteger,
                    @JsonKey(name: r'enum_number') 
    EnumTestEnumNumberEnum?
 enumNumber,
                    @JsonKey(name: r'outerEnum') 
    OuterEnum?
 outerEnum,
                    @JsonKey(name: r'outerEnumInteger') 
    OuterEnumInteger?
 outerEnumInteger,
                    @JsonKey(name: r'outerEnumDefaultValue') 
    OuterEnumDefaultValue?
 outerEnumDefaultValue,
                    @JsonKey(name: r'outerEnumIntegerDefaultValue') 
    OuterEnumIntegerDefaultValue?
 outerEnumIntegerDefaultValue,
        }) = _EnumTest;


        factory EnumTest.fromJson(Map<String, dynamic> json) => _$EnumTestFromJson(json);






}


            
            @JsonEnum(valueField: 'value')
            enum EnumTestEnumStringEnum {
                                    UPPER(value: r'UPPER'),
                        lower(value: r'lower'),
                        empty(value: r''),
                        unknownDefaultOpenApi(value: r'unknown_default_open_api');
                    const EnumTestEnumStringEnum({required this.value});
                    final String value;
            }            
            @JsonEnum(valueField: 'value')
            enum EnumTestEnumStringRequiredEnum {
                                    UPPER(value: r'UPPER'),
                        lower(value: r'lower'),
                        empty(value: r''),
                        unknownDefaultOpenApi(value: r'unknown_default_open_api');
                    const EnumTestEnumStringRequiredEnum({required this.value});
                    final String value;
            }            
            @JsonEnum(valueField: 'value')
            enum EnumTestEnumIntegerEnum {
                                    number1(value: 1),
                        numberNegative1(value: -1),
                        unknownDefaultOpenApi(value: 11184809);
                    const EnumTestEnumIntegerEnum({required this.value});
                    final int value;
            }            
            @JsonEnum(valueField: 'value')
            enum EnumTestEnumNumberEnum {
                                    number1Period1(value: '1.1'),
                        numberNegative1Period2(value: '-1.2'),
                        unknownDefaultOpenApi(value: '11184809');
                    const EnumTestEnumNumberEnum({required this.value});
                    final double value;
            }

//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// EnumArrays
    ///
    /// Properties:
        /// * [justSymbol] 
        /// * [arrayEnum] 

        @freezed
        class EnumArrays with _$EnumArrays {
        const EnumArrays._();
        
        const factory EnumArrays({
                    @JsonKey(name: r'just_symbol') 
    EnumArraysJustSymbolEnum?
 justSymbol,
                    @JsonKey(name: r'array_enum') 
    List<
    EnumArraysArrayEnumEnum?
>?
 arrayEnum,
        }) = _EnumArrays;


        factory EnumArrays.fromJson(Map<String, dynamic> json) => _$EnumArraysFromJson(json);






}


            
            @JsonEnum(valueField: 'value')
            enum EnumArraysJustSymbolEnum {
                                    greaterThanEqual(value: r'>='),
                        dollar(value: r'$'),
                        unknownDefaultOpenApi(value: r'unknown_default_open_api');
                    const EnumArraysJustSymbolEnum({required this.value});
                    final String value;
            }
                
                @JsonEnum(valueField: 'value')
                enum EnumArraysArrayEnumEnum {
                                            fish(value: r'fish'),
                            crab(value: r'crab'),
                            unknownDefaultOpenApi(value: r'unknown_default_open_api');
                        const EnumArraysArrayEnumEnum({required this.value});
                        final String value;
                }

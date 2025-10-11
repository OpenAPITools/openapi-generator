//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Capitalization
    ///
    /// Properties:
        /// * [smallCamel] 
        /// * [capitalCamel] 
        /// * [smallSnake] 
        /// * [capitalSnake] 
        /// * [sCAETHFlowPoints] 
        /// * [ATT_NAME] - Name of the pet 

        @freezed
        class Capitalization with _$Capitalization {
        const Capitalization._();
        
        const factory Capitalization({
                    @JsonKey(name: r'smallCamel') 
    String?
 smallCamel,
                    @JsonKey(name: r'CapitalCamel') 
    String?
 capitalCamel,
                    @JsonKey(name: r'small_Snake') 
    String?
 smallSnake,
                    @JsonKey(name: r'Capital_Snake') 
    String?
 capitalSnake,
                    @JsonKey(name: r'SCA_ETH_Flow_Points') 
    String?
 sCAETHFlowPoints,
                        /// Name of the pet 
            @JsonKey(name: r'ATT_NAME') 
    String?
 ATT_NAME,
        }) = _Capitalization;


        factory Capitalization.fromJson(Map<String, dynamic> json) => _$CapitalizationFromJson(json);






}




//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// AdditionalPropertiesClass
    ///
    /// Properties:
        /// * [mapProperty] 
        /// * [mapOfMapProperty] 

        @freezed
        class AdditionalPropertiesClass with _$AdditionalPropertiesClass {
        const AdditionalPropertiesClass._();
        
        const factory AdditionalPropertiesClass({
                    @JsonKey(name: r'map_property') 
    Map<String, 
    String?
>?
 mapProperty,
                    @JsonKey(name: r'map_of_map_property') 
    Map<String, 
    Map<String, 
    String?
>?
>?
 mapOfMapProperty,
        }) = _AdditionalPropertiesClass;


        factory AdditionalPropertiesClass.fromJson(Map<String, dynamic> json) => _$AdditionalPropertiesClassFromJson(json);






}




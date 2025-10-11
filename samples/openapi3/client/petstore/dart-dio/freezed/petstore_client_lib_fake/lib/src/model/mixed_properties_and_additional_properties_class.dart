//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// MixedPropertiesAndAdditionalPropertiesClass
    ///
    /// Properties:
        /// * [uuid] 
        /// * [dateTime] 
        /// * [map] 

        @freezed
        class MixedPropertiesAndAdditionalPropertiesClass with _$MixedPropertiesAndAdditionalPropertiesClass {
        const MixedPropertiesAndAdditionalPropertiesClass._();
        
        const factory MixedPropertiesAndAdditionalPropertiesClass({
                    @JsonKey(name: r'uuid') 
    String?
 uuid,
                    @JsonKey(name: r'dateTime') 
    DateTime?
 dateTime,
                    @JsonKey(name: r'map') 
    Map<String, 
    Animal?
>?
 map,
        }) = _MixedPropertiesAndAdditionalPropertiesClass;


        factory MixedPropertiesAndAdditionalPropertiesClass.fromJson(Map<String, dynamic> json) => _$MixedPropertiesAndAdditionalPropertiesClassFromJson(json);






}




//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// ObjectWithDeprecatedFields
    ///
    /// Properties:
        /// * [uuid] 
        /// * [id] 
        /// * [deprecatedRef] 
        /// * [bars] 

        @freezed
        class ObjectWithDeprecatedFields with _$ObjectWithDeprecatedFields {
        const ObjectWithDeprecatedFields._();
        
        const factory ObjectWithDeprecatedFields({
                    @JsonKey(name: r'uuid') 
    String?
 uuid,
                    @JsonKey(name: r'id') 
    num?
 id,
                    @JsonKey(name: r'deprecatedRef') 
    DeprecatedObject?
 deprecatedRef,
                    @JsonKey(name: r'bars') 
    List<
    String?
>?
 bars,
        }) = _ObjectWithDeprecatedFields;


        factory ObjectWithDeprecatedFields.fromJson(Map<String, dynamic> json) => _$ObjectWithDeprecatedFieldsFromJson(json);






}




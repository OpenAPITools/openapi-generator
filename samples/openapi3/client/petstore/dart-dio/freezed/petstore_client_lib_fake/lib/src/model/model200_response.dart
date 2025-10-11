//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Model for testing model name starting with number
    ///
    /// Properties:
        /// * [name] 
        /// * [class_] 

        @freezed
        class Model200Response with _$Model200Response {
        const Model200Response._();
        
        const factory Model200Response({
                    @JsonKey(name: r'name') 
    int?
 name,
                    @JsonKey(name: r'class') 
    String?
 class_,
        }) = _Model200Response;


        factory Model200Response.fromJson(Map<String, dynamic> json) => _$Model200ResponseFromJson(json);






}




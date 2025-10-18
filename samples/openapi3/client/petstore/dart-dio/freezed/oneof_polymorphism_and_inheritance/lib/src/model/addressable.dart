//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Base schema for addressable entities
    ///
    /// Properties:
        /// * [href] - Hyperlink reference
        /// * [id] - unique identifier

        @freezed
        class Addressable with _$Addressable {
        const Addressable._();
        
        const factory Addressable({
                        /// Hyperlink reference
            @JsonKey(name: r'href') 
    String?
 href,
                        /// unique identifier
            @JsonKey(name: r'id') 
    String?
 id,
        }) = _Addressable;


        factory Addressable.fromJson(Map<String, dynamic> json) => _$AddressableFromJson(json);






}




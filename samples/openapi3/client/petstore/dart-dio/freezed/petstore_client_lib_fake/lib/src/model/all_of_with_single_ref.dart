//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// AllOfWithSingleRef
    ///
    /// Properties:
        /// * [username] 
        /// * [singleRefType] 

        @freezed
        class AllOfWithSingleRef with _$AllOfWithSingleRef {
        const AllOfWithSingleRef._();
        
        const factory AllOfWithSingleRef({
                    @JsonKey(name: r'username') 
    String?
 username,
                    @JsonKey(name: r'SingleRefType') 
    SingleRefType?
 singleRefType,
        }) = _AllOfWithSingleRef;


        factory AllOfWithSingleRef.fromJson(Map<String, dynamic> json) => _$AllOfWithSingleRefFromJson(json);






}




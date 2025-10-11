//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// OuterComposite
    ///
    /// Properties:
        /// * [myNumber] 
        /// * [myString] 
        /// * [myBoolean] 

        @freezed
        class OuterComposite with _$OuterComposite {
        const OuterComposite._();
        
        const factory OuterComposite({
                    @JsonKey(name: r'my_number') 
    num?
 myNumber,
                    @JsonKey(name: r'my_string') 
    String?
 myString,
                    @JsonKey(name: r'my_boolean') 
    bool?
 myBoolean,
        }) = _OuterComposite;


        factory OuterComposite.fromJson(Map<String, dynamic> json) => _$OuterCompositeFromJson(json);






}




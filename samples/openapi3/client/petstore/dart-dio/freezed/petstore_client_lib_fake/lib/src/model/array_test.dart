//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// ArrayTest
    ///
    /// Properties:
        /// * [arrayOfString] 
        /// * [arrayArrayOfInteger] 
        /// * [arrayArrayOfModel] 

        @freezed
        class ArrayTest with _$ArrayTest {
        const ArrayTest._();
        
        const factory ArrayTest({
                    @JsonKey(name: r'array_of_string') 
    List<
    String?
>?
 arrayOfString,
                    @JsonKey(name: r'array_array_of_integer') 
    List<
    List<
    int?
>?
>?
 arrayArrayOfInteger,
                    @JsonKey(name: r'array_array_of_model') 
    List<
    List<
    ReadOnlyFirst?
>?
>?
 arrayArrayOfModel,
        }) = _ArrayTest;


        factory ArrayTest.fromJson(Map<String, dynamic> json) => _$ArrayTestFromJson(json);






}




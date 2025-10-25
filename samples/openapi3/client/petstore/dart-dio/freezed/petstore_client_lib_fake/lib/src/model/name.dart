//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Model for testing model name same as property name
    ///
    /// Properties:
        /// * [name] 
        /// * [snakeCase] 
        /// * [property] 
        /// * [n123number] 

        @freezed
        class Name with _$Name {
        const Name._();
        
        const factory Name({
                    @JsonKey(name: r'name') 
    required int
 name,
                    @JsonKey(name: r'snake_case') 
    int?
 snakeCase,
                    @JsonKey(name: r'property') 
    String?
 property,
                    @JsonKey(name: r'123Number') 
    int?
 n123number,
        }) = _Name;


        factory Name.fromJson(Map<String, dynamic> json) => _$NameFromJson(json);






}




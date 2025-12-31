//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Dog
    ///
    /// Properties:
        /// * [className] 
        /// * [color] 
        /// * [breed] 


            @freezed
            class Dog with _$Dog {
            const Dog._();
            
            const factory Dog({
                            @JsonKey(name: r'className') 
    required String
 className,
                            @JsonKey(name: r'color') 
    String?
 color,
                            @JsonKey(name: r'breed') 
    String?
 breed,
            }) = _Dog;

            factory Dog.fromJson(Map<String, dynamic> json) => _$DogFromJson(json);






}




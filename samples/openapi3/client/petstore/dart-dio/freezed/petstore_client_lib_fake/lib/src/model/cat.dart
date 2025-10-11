//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Cat
    ///
    /// Properties:
        /// * [className] 
        /// * [color] 
        /// * [declawed] 


            @freezed
            class Cat with _$Cat {
            const Cat._();
            
            const factory Cat({
                            @JsonKey(name: r'className') 
    required String
 className,
                            @JsonKey(name: r'color') 
    String?
 color,
                            @JsonKey(name: r'declawed') 
    bool?
 declawed,
            }) = _Cat;

            factory Cat.fromJson(Map<String, dynamic> json) => _$CatFromJson(json);






}




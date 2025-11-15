//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Category
    ///
    /// Properties:
        /// * [id] 
        /// * [name] 

        @freezed
        class Category with _$Category {
        const Category._();
        
        const factory Category({
                    @JsonKey(name: r'id') 
    int?
 id,
                    @JsonKey(name: r'name') 
    required String
 name,
        }) = _Category;


        factory Category.fromJson(Map<String, dynamic> json) => _$CategoryFromJson(json);






}




//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Pet
    ///
    /// Properties:
        /// * [id] 
        /// * [category] 
        /// * [name] 
        /// * [photoUrls] 
        /// * [tags] 
        /// * [status] - pet status in the store

        @freezed
        class Pet with _$Pet {
        const Pet._();
        
        const factory Pet({
                    @JsonKey(name: r'id') 
    int?
 id,
                    @JsonKey(name: r'category') 
    Category?
 category,
                    @JsonKey(name: r'name') 
    required String
 name,
                    @JsonKey(name: r'photoUrls') 
    required Set<
    String?
>
 photoUrls,
                    @JsonKey(name: r'tags') 
    List<
    Tag?
>?
 tags,
                        /// pet status in the store
            @JsonKey(name: r'status') 
    PetStatusEnum?
 status,
        }) = _Pet;


        factory Pet.fromJson(Map<String, dynamic> json) => _$PetFromJson(json);






}


            /// pet status in the store
            @JsonEnum(valueField: 'value')
            enum PetStatusEnum {
                                    available(value: r'available'),
                        pending(value: r'pending'),
                        sold(value: r'sold'),
                        unknownDefaultOpenApi(value: r'unknown_default_open_api');
                    const PetStatusEnum({required this.value});
                    final String value;
            }

//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// ChildWithNullable
    ///
    /// Properties:
        /// * [type] 
        /// * [nullableProperty] 
        /// * [otherProperty] 


            @freezed
            class ChildWithNullable with _$ChildWithNullable {
            const ChildWithNullable._();
            
            const factory ChildWithNullable({
                            @JsonKey(name: r'type') 
    ChildWithNullableTypeEnum?
 type,
                            @JsonKey(name: r'nullableProperty') 
    String?
 nullableProperty,
                            @JsonKey(name: r'otherProperty') 
    String?
 otherProperty,
            }) = _ChildWithNullable;

            factory ChildWithNullable.fromJson(Map<String, dynamic> json) => _$ChildWithNullableFromJson(json);






}


            
            @JsonEnum(valueField: 'value')
            enum ChildWithNullableTypeEnum {
                                    childWithNullable(value: r'ChildWithNullable'),
                        unknownDefaultOpenApi(value: r'unknown_default_open_api');
                    const ChildWithNullableTypeEnum({required this.value});
                    final String value;
            }

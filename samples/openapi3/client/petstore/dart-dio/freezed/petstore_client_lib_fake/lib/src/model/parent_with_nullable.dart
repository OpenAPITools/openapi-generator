//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// ParentWithNullable
    ///
    /// Properties:
        /// * [type] 
        /// * [nullableProperty] 

@freezed
class ParentWithNullable with _$ParentWithNullable {
const ParentWithNullable._();


                const factory ParentWithNullable.childwithnullable({
                required ChildWithNullable childWithNullable,
                }) = ParentWithNullableChildwithnullable;
            const factory ParentWithNullable.unknown({
            @Default('Json does not satisfy any available types') String message,
            required Map<String, dynamic> json,
            @Default(DeserializationErrorType.UnKnownType)
            DeserializationErrorType errorType,
            @Default(<Type>[]) List<Type> possibleTypes,
                @Default(<ParentWithNullable>[]) List<ParentWithNullable> deserializedModels,
                }) = ParentWithNullableUnknown;




        factory ParentWithNullable.fromJson(Map<String, dynamic> json) {
            switch(json['type']){
                case 'ChildWithNullable':
                return ParentWithNullable.childwithnullable(
                childWithNullable : ChildWithNullable.fromJson(json),
                );
            }
        return ParentWithNullable.unknown(json: json);
        }




      Map<String, dynamic> toJson() {
        return when(
              childwithnullable: (childWithNullable) => childWithNullable.toJson(),
          unknown: (message, json, errorType, possibleTypes, deserializedModels) => <String, dynamic>{},
        );
      }




}



            
@JsonEnum(valueField: 'value')
enum ParentWithNullableTypeEnum {
            childWithNullable(value: r'ChildWithNullable'),
            unknownDefaultOpenApi(value: r'unknown_default_open_api');
        const ParentWithNullableTypeEnum({required this.value});
        final String value;
}


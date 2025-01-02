//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Animal
    ///
    /// Properties:
        /// * [className] 
        /// * [color] 

@freezed
class Animal with _$Animal {
const Animal._();


                const factory Animal.cat({
                required Cat cat,
                }) = AnimalCat;
                const factory Animal.dog({
                required Dog dog,
                }) = AnimalDog;
            const factory Animal.unknown({
            @Default('Json does not satisfy any available types') String message,
            required Map<String, dynamic> json,
            @Default(DeserializationErrorType.UnKnownType)
            DeserializationErrorType errorType,
            @Default(<Type>[]) List<Type> possibleTypes,
                @Default(<Animal>[]) List<Animal> deserializedModels,
                }) = AnimalUnknown;




        factory Animal.fromJson(Map<String, dynamic> json) {
            switch(json['className']){
                case 'CAT':
                return Animal.cat(
                cat : Cat.fromJson(json),
                );
                case 'DOG':
                return Animal.dog(
                dog : Dog.fromJson(json),
                );
            }
        return Animal.unknown(json: json);
        }




      Map<String, dynamic> toJson() {
        return when(
              cat: (cat) => cat.toJson(),
              dog: (dog) => dog.toJson(),
          unknown: (message, json, errorType, possibleTypes, deserializedModels) => <String, dynamic>{},
        );
      }




}





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
            sealed class Animal with _$Animal {
            const Animal._();
            
                                                                                                const factory Animal.asCat({
                            required Cat catValue
                        }) = AnimalAsCat;
                        const factory Animal.asDog({
                            required Dog dogValue
                        }) = AnimalAsDog;
                                    const factory Animal.unknown({
                @Default('Json does not satisfy any available types') String message,
                required Map<String, dynamic> json,
            
                @Default(DeserializationErrorType.UnKnownType)
                DeserializationErrorType errorType,
            
                @Default(<Type>[])
                List<Type> possibleTypes,
            
                @Default(<Animal>[])
                List<Animal> deserializedModels,
            }) = AnimalUnknown;


        factory Animal.fromJson(Map<String, dynamic> json) {
                switch(json['className']){
                        case 'CAT':
                            return Animal.asCat(
                            catValue : Cat.fromJson(json),
                        );
    case 'DOG':
                            return Animal.asDog(
                            dogValue : Dog.fromJson(json),
                        );
                }
                return Animal.unknown(json: json);
        }



        Map<String, dynamic> toJson() {
            return when(
                                                                            asCat: (asCat) => asCat.toJson(),
                            asDog: (asDog) => asDog.toJson(),
                                        unknown: (message, json, errorType, possibleTypes, deserializedModels) => <String, dynamic>{},
            );
        }


}




//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Animal
    ///
    /// Properties:
        /// * [bark] 
        /// * [declawed] 



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
            
                @Default(<Type>[Cat,Dog,])
                List<Type> possibleTypes,
            
                @Default(<Animal>[])
                List<Animal> deserializedModels,
            }) = AnimalUnknown;


            factory Animal.fromJson(Map<String, dynamic> json) {
                    // A discriminator property is not defined in the spec so
                    // we try to parse the json against all the models and try to
                    // return one of the valid model. Note: this approach tries
                    // to return one valid model and if more than one model
                    // is valid it then returns unknown type along with the json so
                    // the consumer can decide which model it is.
                    Animal? deserializedModel;
                    final fromJsonMethods = <FromJsonMethodType <dynamic>>[Cat.fromJson,Dog.fromJson,];
                    final deserializedModels = <Animal>[];
                    for (final fromJsonMethod in fromJsonMethods) {
                        try {
                            final dynamic parsedModel= fromJsonMethod.call(json);
                            // Note following line won't be executed if already the above parsing fails.
                                                        if (parsedModel is Cat) {
                                    deserializedModel =  Animal.asCat(
                                catValue : parsedModel,
                                    );
                                    } else
                                    if (parsedModel is Dog) {
                                    deserializedModel =  Animal.asDog(
                                dogValue : parsedModel,
                                    );
                                    } else
                            {
                            deserializedModel =  Animal.unknown(json: json);
                            }
                            deserializedModels.add(deserializedModel);
                        } catch (e) {
                            // We are suppressing the deserialization error when the json could not
                            // be parsed into one of the model. Because we return [Animal.unknown]
                            // if the deserialization fails.
                        }
                    }
                    // Return an unknown type when the incoming json parses into more than one models.
                    // Since we pass deserializedModels, clients can still use the deserialized model.
                    // EvenThough this is valid for AnyOf types, Dart doesn't have polymorphic types.
                    // So we still return this as an unknown type.
                    if(deserializedModels.length > 1){
                        deserializedModel =  Animal.unknown(
                            json: json,
                            deserializedModels: deserializedModels,
                            errorType: DeserializationErrorType.MoreThanOneTypeSatisfied,
                        );
                    }                    return deserializedModel ?? Animal.unknown(json: json);
            }


            Map<String, dynamic> toJson() {
                return when(
                                    asCat: (asCat) => asCat.toJson(),
                        asDog: (asDog) => asDog.toJson(),
                                                                                            unknown: (message, json, errorType, possibleTypes, deserializedModels) => <String, dynamic>{},
                );
            }

}




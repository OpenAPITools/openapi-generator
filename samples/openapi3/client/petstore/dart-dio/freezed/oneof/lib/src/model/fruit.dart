//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Fruit
    ///
    /// Properties:
        /// * [color] 
        /// * [kind] 
        /// * [count] 
        /// * [sweet] 



            @freezed
            sealed class Fruit with _$Fruit {
            const Fruit._();
            
                            const factory Fruit.asApple({
                    required Apple appleValue
                }) = FruitAsApple;
                const factory Fruit.asBanana({
                    required Banana bananaValue
                }) = FruitAsBanana;
                const factory Fruit.asOrange({
                    required Orange orangeValue
                }) = FruitAsOrange;
                                                                        const factory Fruit.unknown({
                @Default('Json does not satisfy any available types') String message,
                required Map<String, dynamic> json,
            
                @Default(DeserializationErrorType.UnKnownType)
                DeserializationErrorType errorType,
            
                @Default(<Type>[Apple,Banana,Orange,])
                List<Type> possibleTypes,
            
                @Default(<Fruit>[])
                List<Fruit> deserializedModels,
            }) = FruitUnknown;


            factory Fruit.fromJson(Map<String, dynamic> json) {
                    // A discriminator property is not defined in the spec so
                    // we try to parse the json against all the models and try to
                    // return one of the valid model. Note: this approach tries
                    // to return one valid model and if more than one model
                    // is valid it then returns unknown type along with the json so
                    // the consumer can decide which model it is.
                    Fruit? deserializedModel;
                    final fromJsonMethods = <FromJsonMethodType <dynamic>>[Apple.fromJson,Banana.fromJson,Orange.fromJson,];
                    final deserializedModels = <Fruit>[];
                    for (final fromJsonMethod in fromJsonMethods) {
                        try {
                            final dynamic parsedModel= fromJsonMethod.call(json);
                            // Note following line won't be executed if already the above parsing fails.
                                                        if (parsedModel is Apple) {
                                    deserializedModel =  Fruit.asApple(
                                appleValue : parsedModel,
                                    );
                                    } else
                                    if (parsedModel is Banana) {
                                    deserializedModel =  Fruit.asBanana(
                                bananaValue : parsedModel,
                                    );
                                    } else
                                    if (parsedModel is Orange) {
                                    deserializedModel =  Fruit.asOrange(
                                orangeValue : parsedModel,
                                    );
                                    } else
                            {
                            deserializedModel =  Fruit.unknown(json: json);
                            }
                            deserializedModels.add(deserializedModel);
                        } catch (e) {
                            // We are suppressing the deserialization error when the json could not
                            // be parsed into one of the model. Because we return [Fruit.unknown]
                            // if the deserialization fails.
                        }
                    }
                    // Return an unknown type when the incoming json parses into more than one models.
                    // Since we pass deserializedModels, clients can still use the deserialized model.
                    // EvenThough this is valid for AnyOf types, Dart doesn't have polymorphic types.
                    // So we still return this as an unknown type.
                    if(deserializedModels.length > 1){
                        deserializedModel =  Fruit.unknown(
                            json: json,
                            deserializedModels: deserializedModels,
                            errorType: DeserializationErrorType.MoreThanOneTypeSatisfied,
                        );
                    }                    return deserializedModel ?? Fruit.unknown(json: json);
            }


            Map<String, dynamic> toJson() {
                return when(
                                    asApple: (asApple) => asApple.toJson(),
                        asBanana: (asBanana) => asBanana.toJson(),
                        asOrange: (asOrange) => asOrange.toJson(),
                                                                                            unknown: (message, json, errorType, possibleTypes, deserializedModels) => <String, dynamic>{},
                );
            }

}




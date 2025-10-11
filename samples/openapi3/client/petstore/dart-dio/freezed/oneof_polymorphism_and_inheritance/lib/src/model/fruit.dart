//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Fruit
    ///
    /// Properties:
        /// * [fruitType] 
        /// * [seeds] 
        /// * [length] 



            @freezed
            sealed class Fruit with _$Fruit {
            const Fruit._();
            
                            const factory Fruit.asApple({
                    required Apple appleValue
                }) = FruitAsApple;
                const factory Fruit.asBanana({
                    required Banana bananaValue
                }) = FruitAsBanana;
                                                                        const factory Fruit.unknown({
                @Default('Json does not satisfy any available types') String message,
                required Map<String, dynamic> json,
            
                @Default(DeserializationErrorType.UnKnownType)
                DeserializationErrorType errorType,
            
                @Default(<Type>[Apple,Banana,])
                List<Type> possibleTypes,
            
                @Default(<Fruit>[])
                List<Fruit> deserializedModels,
            }) = FruitUnknown;

        factory Fruit.fromJson(Map<String, dynamic> json) {
                switch(json['fruitType']){
                        case 'APPLE':
                            return Fruit.asApple(
                            appleValue : Apple.fromJson(json),
                        );
    case 'BANANA':
                            return Fruit.asBanana(
                            bananaValue : Banana.fromJson(json),
                        );
                }
                return Fruit.unknown(json: json);
        }


        Map<String, dynamic> toJson() {
            return when(
                            asApple: (asApple) => asApple.toJson(),
                    asBanana: (asBanana) => asBanana.toJson(),
                                                                unknown: (message, json, errorType, possibleTypes, deserializedModels) => <String, dynamic>{},
            );
        }


}




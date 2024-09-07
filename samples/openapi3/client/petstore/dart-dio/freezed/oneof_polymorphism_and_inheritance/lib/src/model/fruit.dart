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
class Fruit with _$Fruit {
const Fruit._();




                const factory Fruit.apple({
                    required Apple apple,
                }) = FruitApple;
                const factory Fruit.banana({
                    required Banana banana,
                }) = FruitBanana;
            const factory Fruit.unknown({
                @Default('Json does not satisfy any available types') String message,
                required Map<String, dynamic> json,
                @Default(DeserializationErrorType.UnKnownType)
                DeserializationErrorType errorType,
                @Default(<Type>[Apple,Banana,]) List<Type> possibleTypes,
                @Default(<Fruit>[]) List<Fruit> deserializedModels,
            }) = FruitUnknown;



    factory Fruit.fromJson(Map<String, dynamic> json) {
            switch(json['fruitType']){
                    case 'APPLE':
                        return Fruit.apple(
                            apple : Apple.fromJson(json),
                        );
                    case 'BANANA':
                        return Fruit.banana(
                            banana : Banana.fromJson(json),
                        );
            }
        return Fruit.unknown(json: json);
    }



      Map<String, dynamic> toJson() {
        return when(
              apple: (apple) => apple.toJson(),
              banana: (banana) => banana.toJson(),
          unknown: (message, json, errorType, possibleTypes, deserializedModels) => <String, dynamic>{},
        );
      }




}





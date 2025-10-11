//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Entity
    ///
    /// Properties:
        /// * [href] - Hyperlink reference
        /// * [id] - unique identifier
        /// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
        /// * [atBaseType] - When sub-classing, this defines the super-class
        /// * [atType] - When sub-classing, this defines the sub-class Extensible name



            @freezed
            sealed class Entity with _$Entity {
            const Entity._();
            
                                                                                                const factory Entity.asBar({
                            required Bar barValue
                        }) = EntityAsBar;
                        const factory Entity.asBarCreate({
                            required BarCreate barCreateValue
                        }) = EntityAsBarCreate;
                        const factory Entity.asFoo({
                            required Foo fooValue
                        }) = EntityAsFoo;
                        const factory Entity.asPasta({
                            required Pasta pastaValue
                        }) = EntityAsPasta;
                        const factory Entity.asPizza({
                            required Pizza pizzaValue
                        }) = EntityAsPizza;
                        const factory Entity.asPizzaSpeziale({
                            required PizzaSpeziale pizzaSpezialeValue
                        }) = EntityAsPizzaSpeziale;
                                    const factory Entity.unknown({
                @Default('Json does not satisfy any available types') String message,
                required Map<String, dynamic> json,
            
                @Default(DeserializationErrorType.UnKnownType)
                DeserializationErrorType errorType,
            
                @Default(<Type>[])
                List<Type> possibleTypes,
            
                @Default(<Entity>[])
                List<Entity> deserializedModels,
            }) = EntityUnknown;

        factory Entity.fromJson(Map<String, dynamic> json) {
                switch(json['@type']){
                        case 'Bar':
                            return Entity.asBar(
                            barValue : Bar.fromJson(json),
                        );
    case 'Bar_Create':
                            return Entity.asBarCreate(
                            barCreateValue : BarCreate.fromJson(json),
                        );
    case 'Foo':
                            return Entity.asFoo(
                            fooValue : Foo.fromJson(json),
                        );
    case 'Pasta':
                            return Entity.asPasta(
                            pastaValue : Pasta.fromJson(json),
                        );
    case 'Pizza':
                            return Entity.asPizza(
                            pizzaValue : Pizza.fromJson(json),
                        );
    case 'PizzaSpeziale':
                            return Entity.asPizzaSpeziale(
                            pizzaSpezialeValue : PizzaSpeziale.fromJson(json),
                        );
                }
                return Entity.unknown(json: json);
        }


        Map<String, dynamic> toJson() {
            return when(
                                                                            asBar: (asBar) => asBar.toJson(),
                            asBarCreate: (asBarCreate) => asBarCreate.toJson(),
                            asFoo: (asFoo) => asFoo.toJson(),
                            asPasta: (asPasta) => asPasta.toJson(),
                            asPizza: (asPizza) => asPizza.toJson(),
                            asPizzaSpeziale: (asPizzaSpeziale) => asPizzaSpeziale.toJson(),
                                        unknown: (message, json, errorType, possibleTypes, deserializedModels) => <String, dynamic>{},
            );
        }


}




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
class Entity with _$Entity {
const Entity._();




                const factory Entity.bar({
                    required Bar bar,
                }) = EntityBar;
                const factory Entity.barCreate({
                    required BarCreate barCreate,
                }) = EntityBar_create;
                const factory Entity.foo({
                    required Foo foo,
                }) = EntityFoo;
                const factory Entity.pasta({
                    required Pasta pasta,
                }) = EntityPasta;
                const factory Entity.pizza({
                    required Pizza pizza,
                }) = EntityPizza;
                const factory Entity.pizzaspeziale({
                    required PizzaSpeziale pizzaSpeziale,
                }) = EntityPizzaspeziale;
            const factory Entity.unknown({
                @Default('Json does not satisfy any available types') String message,
                required Map<String, dynamic> json,
                @Default(DeserializationErrorType.UnKnownType)
                DeserializationErrorType errorType,
                @Default(<Type>[]) List<Type> possibleTypes,
                @Default(<Entity>[]) List<Entity> deserializedModels,
            }) = EntityUnknown;



    factory Entity.fromJson(Map<String, dynamic> json) {
            switch(json['@type']){
                    case 'Bar':
                        return Entity.bar(
                            bar : Bar.fromJson(json),
                        );
                    case 'Bar_Create':
                        return Entity.barCreate(
                            barCreate : BarCreate.fromJson(json),
                        );
                    case 'Foo':
                        return Entity.foo(
                            foo : Foo.fromJson(json),
                        );
                    case 'Pasta':
                        return Entity.pasta(
                            pasta : Pasta.fromJson(json),
                        );
                    case 'Pizza':
                        return Entity.pizza(
                            pizza : Pizza.fromJson(json),
                        );
                    case 'PizzaSpeziale':
                        return Entity.pizzaspeziale(
                            pizzaSpeziale : PizzaSpeziale.fromJson(json),
                        );
            }
        return Entity.unknown(json: json);
    }



      Map<String, dynamic> toJson() {
        return when(
              bar: (bar) => bar.toJson(),
              barCreate: (barCreate) => barCreate.toJson(),
              foo: (foo) => foo.toJson(),
              pasta: (pasta) => pasta.toJson(),
              pizza: (pizza) => pizza.toJson(),
              pizzaspeziale: (pizzaSpeziale) => pizzaSpeziale.toJson(),
          unknown: (message, json, errorType, possibleTypes, deserializedModels) => <String, dynamic>{},
        );
      }




}





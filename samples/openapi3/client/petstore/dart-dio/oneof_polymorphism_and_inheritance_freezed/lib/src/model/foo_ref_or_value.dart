//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// FooRefOrValue
    ///
    /// Properties:
        /// * [fooPropA] 
        /// * [fooPropB] 
        /// * [href] - Hyperlink reference
        /// * [id] - unique identifier
        /// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
        /// * [atBaseType] - When sub-classing, this defines the super-class
        /// * [atType] - When sub-classing, this defines the sub-class Extensible name
        /// * [foorefPropA] 
        /// * [name] - Name of the related entity.
        /// * [atReferredType] - The actual type of the target instance when needed for disambiguation.

@freezed
class FooRefOrValue with _$FooRefOrValue {
const FooRefOrValue._();




                    const factory FooRefOrValue.asFoo({
                        required Foo fooValue
                    }) = FooRefOrValueAsFoo;
                    const factory FooRefOrValue.asFooRef({
                        required FooRef fooRefValue
                    }) = FooRefOrValueAsFooRef;
                const factory FooRefOrValue.unknown({
                    @Default('Json does not satisfy any available types') String message,
                    required Map<String, dynamic> json,
                    @Default(DeserializationErrorType.UnKnownType)
                    DeserializationErrorType errorType,
                    @Default(<Type>[Foo,FooRef,]) List<Type> possibleTypes,
                    @Default(<FooRefOrValue>[]) List<FooRefOrValue> deserializedModels,
                }) = FooRefOrValueUnknown;




            factory FooRefOrValue.fromJson(Map<String, dynamic> json) {
                FooRefOrValue? deserializedModel;
                    // A discriminator property is specified but no mapping
                    // is provided in the spec, so we expect the property to
                    // have the value of the name of the model. Model prefix &
                    // suffix are ignored, as this is not known by the api provider
                    switch(json['@type']){
                        case 'Foo':
                            deserializedModel =  FooRefOrValue.asFoo(
                            fooValue : Foo.fromJson(json),
                            );
                            break;
                        case 'FooRef':
                            deserializedModel =  FooRefOrValue.asFooRef(
                            fooRefValue : FooRef.fromJson(json),
                            );
                            break;
                        default:
                            break;
                    }


                return deserializedModel ?? FooRefOrValue.unknown(json: json);
            }



            Map<String, dynamic> toJson() {
                return when(
                        asFoo: (asFoo) => asFoo.toJson(),
                        asFooRef: (asFooRef) => asFooRef.toJson(),
                    unknown: (message, json, errorType, possibleTypes, deserializedModels) => <String, dynamic>{},
                );
            }



}





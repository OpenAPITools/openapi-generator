//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// BarRefOrValue
    ///
    /// Properties:
        /// * [id] - unique identifier
        /// * [barPropA] 
        /// * [fooPropB] 
        /// * [foo] 
        /// * [href] - Hyperlink reference
        /// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
        /// * [atBaseType] - When sub-classing, this defines the super-class
        /// * [atType] - When sub-classing, this defines the sub-class Extensible name
        /// * [name] - Name of the related entity.
        /// * [atReferredType] - The actual type of the target instance when needed for disambiguation.



            @freezed
            sealed class BarRefOrValue with _$BarRefOrValue {
            const BarRefOrValue._();
            
                            const factory BarRefOrValue.asBar({
                    required Bar barValue
                }) = BarRefOrValueAsBar;
                const factory BarRefOrValue.asBarRef({
                    required BarRef barRefValue
                }) = BarRefOrValueAsBarRef;
                                                                        const factory BarRefOrValue.unknown({
                @Default('Json does not satisfy any available types') String message,
                required Map<String, dynamic> json,
            
                @Default(DeserializationErrorType.UnKnownType)
                DeserializationErrorType errorType,
            
                @Default(<Type>[Bar,BarRef,])
                List<Type> possibleTypes,
            
                @Default(<BarRefOrValue>[])
                List<BarRefOrValue> deserializedModels,
            }) = BarRefOrValueUnknown;


            factory BarRefOrValue.fromJson(Map<String, dynamic> json) {
                    // A discriminator property is not defined in the spec so
                    // we try to parse the json against all the models and try to
                    // return one of the valid model. Note: this approach tries
                    // to return one valid model and if more than one model
                    // is valid it then returns unknown type along with the json so
                    // the consumer can decide which model it is.
                    BarRefOrValue? deserializedModel;
                    final fromJsonMethods = <FromJsonMethodType <dynamic>>[Bar.fromJson,BarRef.fromJson,];
                    final deserializedModels = <BarRefOrValue>[];
                    for (final fromJsonMethod in fromJsonMethods) {
                        try {
                            final dynamic parsedModel= fromJsonMethod.call(json);
                            // Note following line won't be executed if already the above parsing fails.
                                                        if (parsedModel is Bar) {
                                    deserializedModel =  BarRefOrValue.asBar(
                                barValue : parsedModel,
                                    );
                                    } else
                                    if (parsedModel is BarRef) {
                                    deserializedModel =  BarRefOrValue.asBarRef(
                                barRefValue : parsedModel,
                                    );
                                    } else
                            {
                            deserializedModel =  BarRefOrValue.unknown(json: json);
                            }
                            deserializedModels.add(deserializedModel);
                        } catch (e) {
                            // We are suppressing the deserialization error when the json could not
                            // be parsed into one of the model. Because we return [BarRefOrValue.unknown]
                            // if the deserialization fails.
                        }
                    }
                    // Return an unknown type when the incoming json parses into more than one models.
                    // Since we pass deserializedModels, clients can still use the deserialized model.
                    // EvenThough this is valid for AnyOf types, Dart doesn't have polymorphic types.
                    // So we still return this as an unknown type.
                    if(deserializedModels.length > 1){
                        deserializedModel =  BarRefOrValue.unknown(
                            json: json,
                            deserializedModels: deserializedModels,
                            errorType: DeserializationErrorType.MoreThanOneTypeSatisfied,
                        );
                    }                    return deserializedModel ?? BarRefOrValue.unknown(json: json);
            }


            Map<String, dynamic> toJson() {
                return when(
                                    asBar: (asBar) => asBar.toJson(),
                        asBarRef: (asBarRef) => asBarRef.toJson(),
                                                                                            unknown: (message, json, errorType, possibleTypes, deserializedModels) => <String, dynamic>{},
                );
            }

}




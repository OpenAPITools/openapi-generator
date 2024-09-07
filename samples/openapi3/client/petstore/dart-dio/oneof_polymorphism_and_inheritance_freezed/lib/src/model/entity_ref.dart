//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Entity reference schema to be use for all entityRef class.
    ///
    /// Properties:
        /// * [name] - Name of the related entity.
        /// * [atReferredType] - The actual type of the target instance when needed for disambiguation.
        /// * [href] - Hyperlink reference
        /// * [id] - unique identifier
        /// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
        /// * [atBaseType] - When sub-classing, this defines the super-class
        /// * [atType] - When sub-classing, this defines the sub-class Extensible name

@freezed
class EntityRef with _$EntityRef {
const EntityRef._();




                const factory EntityRef.barref({
                    required BarRef barRef,
                }) = EntityRefBarref;
                const factory EntityRef.fooref({
                    required FooRef fooRef,
                }) = EntityRefFooref;
            const factory EntityRef.unknown({
                @Default('Json does not satisfy any available types') String message,
                required Map<String, dynamic> json,
                @Default(DeserializationErrorType.UnKnownType)
                DeserializationErrorType errorType,
                @Default(<Type>[]) List<Type> possibleTypes,
                @Default(<EntityRef>[]) List<EntityRef> deserializedModels,
            }) = EntityRefUnknown;



    factory EntityRef.fromJson(Map<String, dynamic> json) {
            switch(json['@type']){
                    case 'BarRef':
                        return EntityRef.barref(
                            barRef : BarRef.fromJson(json),
                        );
                    case 'FooRef':
                        return EntityRef.fooref(
                            fooRef : FooRef.fromJson(json),
                        );
            }
        return EntityRef.unknown(json: json);
    }



      Map<String, dynamic> toJson() {
        return when(
              barref: (barRef) => barRef.toJson(),
              fooref: (fooRef) => fooRef.toJson(),
          unknown: (message, json, errorType, possibleTypes, deserializedModels) => <String, dynamic>{},
        );
      }




}





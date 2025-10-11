//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Extensible
    ///
    /// Properties:
        /// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
        /// * [atBaseType] - When sub-classing, this defines the super-class
        /// * [atType] - When sub-classing, this defines the sub-class Extensible name

        @freezed
        class Extensible with _$Extensible {
        const Extensible._();
        
        const factory Extensible({
                        /// A URI to a JSON-Schema file that defines additional attributes and relationships
            @JsonKey(name: r'@schemaLocation') 
    String?
 atSchemaLocation,
                        /// When sub-classing, this defines the super-class
            @JsonKey(name: r'@baseType') 
    String?
 atBaseType,
                        /// When sub-classing, this defines the sub-class Extensible name
            @JsonKey(name: r'@type') 
    required String
 atType,
        }) = _Extensible;


        factory Extensible.fromJson(Map<String, dynamic> json) => _$ExtensibleFromJson(json);






}




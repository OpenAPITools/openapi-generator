//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// BarCreate
    ///
    /// Properties:
        /// * [barPropA] 
        /// * [fooPropB] 
        /// * [foo] 
        /// * [href] - Hyperlink reference
        /// * [id] - unique identifier
        /// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
        /// * [atBaseType] - When sub-classing, this defines the super-class
        /// * [atType] - When sub-classing, this defines the sub-class Extensible name


            @freezed
            class BarCreate with _$BarCreate {
            const BarCreate._();
            
            const factory BarCreate({
                            @JsonKey(name: r'barPropA') 
    String?
 barPropA,
                            @JsonKey(name: r'fooPropB') 
    String?
 fooPropB,
                            @JsonKey(name: r'foo') 
    FooRefOrValue?
 foo,
                                /// Hyperlink reference
                @JsonKey(name: r'href') 
    String?
 href,
                                /// unique identifier
                @JsonKey(name: r'id') 
    String?
 id,
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
            }) = _BarCreate;

            factory BarCreate.fromJson(Map<String, dynamic> json) => _$BarCreateFromJson(json);






}




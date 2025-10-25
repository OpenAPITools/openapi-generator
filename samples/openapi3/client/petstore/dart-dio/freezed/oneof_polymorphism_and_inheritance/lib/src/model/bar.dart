//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Bar
    ///
    /// Properties:
        /// * [id] 
        /// * [barPropA] 
        /// * [fooPropB] 
        /// * [foo] 
        /// * [href] - Hyperlink reference
        /// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
        /// * [atBaseType] - When sub-classing, this defines the super-class
        /// * [atType] - When sub-classing, this defines the sub-class Extensible name


            @freezed
            class Bar with _$Bar {
            const Bar._();
            
            const factory Bar({
                            @JsonKey(name: r'id') 
    required String
 id,
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
            }) = _Bar;

            factory Bar.fromJson(Map<String, dynamic> json) => _$BarFromJson(json);






}




// Model reflection

part of 'upload_file_with_required_file_request.dart';


//class reflection

class UploadFileWithRequiredFileRequestReflection extends ClassReflection<UploadFileWithRequiredFileRequest> {
  static UploadFileWithRequiredFileRequestReflection instanceGetter() => instance;
  static const instance = UploadFileWithRequiredFileRequestReflection._(
    modelName: r'uploadFileWithRequiredFile_request',
    className: r'UploadFileWithRequiredFileRequest',
    additionalMetadataPart: PropertyReflection<UploadFileWithRequiredFileRequest, UndefinedWrapper<
            String

>>(
      dartName: r'additionalMetadata',
      nullable: false,
      required: false,
      oasName: r'additionalMetadata',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _additionalMetadataGetter,
      setter: _additionalMetadataSetter,
    ),
    requiredFilePart: PropertyReflection<UploadFileWithRequiredFileRequest, 
            XFile

>(
      dartName: r'requiredFile',
      nullable: false,
      required: true,
      oasName: r'requiredFile',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _requiredFileGetter,
      setter: _requiredFileSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<UploadFileWithRequiredFileRequest, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const UploadFileWithRequiredFileRequestReflection._({
    required this.modelName,
    required this.className,
    required this.additionalMetadataPart,
    required this.requiredFilePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<UploadFileWithRequiredFileRequest, UndefinedWrapper<
            String

>> additionalMetadataPart;
  static UndefinedWrapper<
            String

> _additionalMetadataGetter(UploadFileWithRequiredFileRequest parent) {
    return parent.additionalMetadata;
  }
  static void _additionalMetadataSetter(UploadFileWithRequiredFileRequest parent, UndefinedWrapper<
            String

> value) {
    parent.additionalMetadata = value;
  }
  final PropertyReflection<UploadFileWithRequiredFileRequest, 
            XFile

> requiredFilePart;
  static 
            XFile

 _requiredFileGetter(UploadFileWithRequiredFileRequest parent) {
    return parent.requiredFile;
  }
  static void _requiredFileSetter(UploadFileWithRequiredFileRequest parent, 
            XFile

 value) {
    parent.requiredFile = value;
  }



  @override
  final Map<String, ClassReflection> discriminatorMappings;
  @override
  final Map<String, ClassReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;


  @override
  List<PropertyReflection<UploadFileWithRequiredFileRequest, dynamic>> get properties => [
    additionalMetadataPart,
requiredFilePart,
  ];

  final AdditionalPropertiesReflection<UploadFileWithRequiredFileRequest, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<UploadFileWithRequiredFileRequest, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<UploadFileWithRequiredFileRequest, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => UploadFileWithRequiredFileRequest.canDeserialize(src);
  @override
  UploadFileWithRequiredFileRequest Function(Object? src) get deserializeFunction =>
      (src) => UploadFileWithRequiredFileRequest.deserialize(src);

  @override
  Object? Function(UploadFileWithRequiredFileRequest src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of UploadFileWithRequiredFileRequest.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  UploadFileWithRequiredFileRequest example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
        discriminatorExampleResults = const {},}) {
    final _reflection = this;
    final actualDiscriminators = discriminators ?? _reflection.aggregatedDiscriminators;
    discriminatorExampleResults = Map.from(discriminatorExampleResults);
    for (final MapEntry(key: propName, value: mappings) in actualDiscriminators.entries) {
      if (discriminatorExampleResults.containsKey(propName)) {
        continue;
      }
      final r =  exampleDiscriminator(mappings);
      if (r != null){
        discriminatorExampleResults[propName] = r;
      }
    }

    final exampleResult = UploadFileWithRequiredFileRequest(
      additionalMetadata: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[additionalMetadataPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      requiredFile: () {
        var result = 


            
            


    
    exampleXFile()


;
        return result;
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class UploadFileWithRequiredFileRequestXmlReflection {
    const UploadFileWithRequiredFileRequestXmlReflection();
}


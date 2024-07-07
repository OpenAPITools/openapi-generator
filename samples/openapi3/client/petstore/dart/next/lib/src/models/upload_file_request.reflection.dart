// Model reflection

part of 'upload_file_request.dart';


//class reflection

class UploadFileRequestReflection extends ClassReflection<UploadFileRequest> {
  static UploadFileRequestReflection instanceGetter() => instance;
  static const instance = UploadFileRequestReflection._(
    modelName: r'uploadFile_request',
    className: r'UploadFileRequest',
    additionalMetadataPart: PropertyReflection<UploadFileRequest, UndefinedWrapper<
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
    filePart: PropertyReflection<UploadFileRequest, UndefinedWrapper<
            XFile
>>(
      dartName: r'file',
      nullable: false,
      required: false,
      oasName: r'file',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _fileGetter,
      setter: _fileSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<UploadFileRequest, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const UploadFileRequestReflection._({
    required this.modelName,
    required this.className,
    required this.additionalMetadataPart,
    required this.filePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<UploadFileRequest, UndefinedWrapper<
            String
>> additionalMetadataPart;
  static UndefinedWrapper<
            String
> _additionalMetadataGetter(UploadFileRequest parent) {
    return parent.additionalMetadata;
  }
  static void _additionalMetadataSetter(UploadFileRequest parent, UndefinedWrapper<
            String
> value) {
    parent.additionalMetadata = value;
  }
  final PropertyReflection<UploadFileRequest, UndefinedWrapper<
            XFile
>> filePart;
  static UndefinedWrapper<
            XFile
> _fileGetter(UploadFileRequest parent) {
    return parent.file;
  }
  static void _fileSetter(UploadFileRequest parent, UndefinedWrapper<
            XFile
> value) {
    parent.file = value;
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
  List<PropertyReflection<UploadFileRequest, dynamic>> get properties => [
    additionalMetadataPart,
filePart,
  ];

  final AdditionalPropertiesReflection<UploadFileRequest, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<UploadFileRequest, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<UploadFileRequest, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => UploadFileRequest.canDeserialize(src);
  @override
  UploadFileRequest Function(Object? src) get deserializeFunction =>
      (src) => UploadFileRequest.deserialize(src);

  @override
  Object? Function(UploadFileRequest src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of UploadFileRequest.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  UploadFileRequest example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return UploadFileRequest(
      additionalMetadata: () {
        PartReflection? _partReflection = _reflection.additionalMetadataPart;
        
        final disc = discriminators[r'additionalMetadata'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return UndefinedWrapper(result);
          }
        }
        
        return UndefinedWrapper(


            
            


    
    exampleString()


);
      }(),
      file: () {
        PartReflection? _partReflection = _reflection.filePart;
        
        return UndefinedWrapper(


            
            


    
    exampleXFile()


);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class UploadFileRequestXmlReflection {
    const UploadFileRequestXmlReflection();
}


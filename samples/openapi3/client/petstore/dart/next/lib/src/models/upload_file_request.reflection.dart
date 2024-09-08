// Model reflection

part of 'upload_file_request.dart';


//class reflection

class UploadFileRequestReflection extends ModelReflection<UploadFileRequest> {
  static UploadFileRequestReflection instanceGetter() => instance;
  static const instance = UploadFileRequestReflection._(
    modelName: r'uploadFile_request',
    className: r'UploadFileRequest',
    xml: XmlReflection(
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_additionalMetadataGetter),
      setter: FunctionWrapper2(_additionalMetadataSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_fileGetter),
      setter: FunctionWrapper2(_fileSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forXFile
        
,
)
),
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(ObjectReflection()
),
)
,
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const UploadFileRequestReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
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
  final Map<String, ModelReflection> discriminatorMappings;
  @override
  final Map<String, ModelReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;
  @override
  final XmlReflection xml;

  @override
  List<PropertyReflection<UploadFileRequest, dynamic>> get properties => [
    additionalMetadataPart,
filePart,
  ];

  @override
  final AdditionalPropertiesPart<UploadFileRequest, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(UploadFileRequest instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(UploadFileRequest instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<UploadFileRequest, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  UploadFileRequest empty() {
    return UploadFileRequest(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is UploadFileRequestReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



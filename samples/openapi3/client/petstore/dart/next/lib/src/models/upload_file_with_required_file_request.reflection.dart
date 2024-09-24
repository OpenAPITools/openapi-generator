// Model reflection

part of 'upload_file_with_required_file_request.dart';


//class reflection

class UploadFileWithRequiredFileRequestReflection extends ModelReflection<UploadFileWithRequiredFileRequest> {
  static UploadFileWithRequiredFileRequestReflection instanceGetter() => instance;
  static const instance = UploadFileWithRequiredFileRequestReflection._(
    modelName: r'uploadFileWithRequiredFile_request',
    className: r'UploadFileWithRequiredFileRequest',
    xml: XmlReflection(
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_requiredFileGetter),
      setter: FunctionWrapper2(_requiredFileSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forXFile
        
,
)
,
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
  const UploadFileWithRequiredFileRequestReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
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
  List<PropertyReflection<UploadFileWithRequiredFileRequest, dynamic>> get properties => [
    additionalMetadataPart,
requiredFilePart,
  ];

  @override
  final AdditionalPropertiesPart<UploadFileWithRequiredFileRequest, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(UploadFileWithRequiredFileRequest instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(UploadFileWithRequiredFileRequest instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<UploadFileWithRequiredFileRequest, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  UploadFileWithRequiredFileRequest empty() {
    return UploadFileWithRequiredFileRequest(
      requiredFile: requiredFilePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is UploadFileWithRequiredFileRequestReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



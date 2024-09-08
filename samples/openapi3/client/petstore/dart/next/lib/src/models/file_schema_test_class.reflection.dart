// Model reflection

part of 'file_schema_test_class.dart';


//class reflection

class FileSchemaTestClassReflection extends ModelReflection<FileSchemaTestClass> {
  static FileSchemaTestClassReflection instanceGetter() => instance;
  static const instance = FileSchemaTestClassReflection._(
    modelName: r'FileSchemaTestClass',
    className: r'FileSchemaTestClass',
    xml: XmlReflection(
),
    filePart: PropertyReflection<FileSchemaTestClass, UndefinedWrapper<
            File
>>(
      dartName: r'file',
      nullable: false,
      required: false,
      oasName: r'file',
      oasType: r'File',
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
    
            
        
        
            
                File.$reflection
        
,
)
),
    ),
    filesPart: PropertyReflection<FileSchemaTestClass, UndefinedWrapper<
    List<
        
            File
>
>>(
      dartName: r'files',
      nullable: false,
      required: false,
      oasName: r'files',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_filesGetter),
      setter: FunctionWrapper2(_filesSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                File.$reflection
        
,
)
)
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
  const FileSchemaTestClassReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.filePart,
    required this.filesPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<FileSchemaTestClass, UndefinedWrapper<
            File
>> filePart;
  static UndefinedWrapper<
            File
> _fileGetter(FileSchemaTestClass parent) {
    return parent.file;
  }
  static void _fileSetter(FileSchemaTestClass parent, UndefinedWrapper<
            File
> value) {
    parent.file = value;
  }

  final PropertyReflection<FileSchemaTestClass, UndefinedWrapper<
    List<
        
            File
>
>> filesPart;
  static UndefinedWrapper<
    List<
        
            File
>
> _filesGetter(FileSchemaTestClass parent) {
    return parent.files;
  }
  static void _filesSetter(FileSchemaTestClass parent, UndefinedWrapper<
    List<
        
            File
>
> value) {
    parent.files = value;
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
  List<PropertyReflection<FileSchemaTestClass, dynamic>> get properties => [
    filePart,
filesPart,
  ];

  @override
  final AdditionalPropertiesPart<FileSchemaTestClass, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(FileSchemaTestClass instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(FileSchemaTestClass instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<FileSchemaTestClass, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  FileSchemaTestClass empty() {
    return FileSchemaTestClass(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is FileSchemaTestClassReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



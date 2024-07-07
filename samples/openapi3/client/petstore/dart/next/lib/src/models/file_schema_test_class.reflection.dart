// Model reflection

part of 'file_schema_test_class.dart';


//class reflection

class FileSchemaTestClassReflection extends ClassReflection<FileSchemaTestClass> {
  static FileSchemaTestClassReflection instanceGetter() => instance;
  static const instance = FileSchemaTestClassReflection._(
    modelName: r'FileSchemaTestClass',
    className: r'FileSchemaTestClass',
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
      classReflection: FileReflection.instance,
      getter: _fileGetter,
      setter: _fileSetter,
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
      itemsReflection: ItemsReflection<FileSchemaTestClass, 
            File
>(parentReflectionGetter: instanceGetter,classReflection: FileReflection.instance,),
      getter: _filesGetter,
      setter: _filesSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FileSchemaTestClass, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const FileSchemaTestClassReflection._({
    required this.modelName,
    required this.className,
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
  List<PropertyReflection<FileSchemaTestClass, dynamic>> get properties => [
    filePart,
filesPart,
  ];

  final AdditionalPropertiesReflection<FileSchemaTestClass, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<FileSchemaTestClass, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<FileSchemaTestClass, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FileSchemaTestClass.canDeserialize(src);
  @override
  FileSchemaTestClass Function(Object? src) get deserializeFunction =>
      (src) => FileSchemaTestClass.deserialize(src);

  @override
  Object? Function(FileSchemaTestClass src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of FileSchemaTestClass.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  FileSchemaTestClass example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return FileSchemaTestClass(
      file: () {
        PartReflection? _partReflection = _reflection.filePart;
        
        return UndefinedWrapper(


            
            


    File.$reflection.example()
    


);
      }(),
      files: () {
        PartReflection? _partReflection = _reflection.filesPart;
        
        return UndefinedWrapper(


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    File.$reflection.example()
    


; })



);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class FileSchemaTestClassXmlReflection {
    const FileSchemaTestClassXmlReflection();
}


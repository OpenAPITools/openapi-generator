// Model reflection

part of 'file.dart';


//class reflection

class FileReflection extends ClassReflection<File> {
  static FileReflection instanceGetter() => instance;
  static const instance = FileReflection._(
    modelName: r'File',
    className: r'File',
    sourceURIPart: PropertyReflection<File, UndefinedWrapper<
            String
>>(
      dartName: r'sourceURI',
      nullable: false,
      required: false,
      oasName: r'sourceURI',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _sourceURIGetter,
      setter: _sourceURISetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<File, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const FileReflection._({
    required this.modelName,
    required this.className,
    required this.sourceURIPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<File, UndefinedWrapper<
            String
>> sourceURIPart;
  static UndefinedWrapper<
            String
> _sourceURIGetter(File parent) {
    return parent.sourceURI;
  }
  static void _sourceURISetter(File parent, UndefinedWrapper<
            String
> value) {
    parent.sourceURI = value;
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
  List<PropertyReflection<File, dynamic>> get properties => [
    sourceURIPart,
  ];

  final AdditionalPropertiesReflection<File, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<File, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<File, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => File.canDeserialize(src);
  @override
  File Function(Object? src) get deserializeFunction =>
      (src) => File.deserialize(src);

  @override
  Object? Function(File src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of File.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  File example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return File(
      sourceURI: () {
        PartReflection? _partReflection = _reflection.sourceURIPart;
        
        final disc = discriminators[r'sourceURI'];
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
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class FileXmlReflection {
    const FileXmlReflection();
}


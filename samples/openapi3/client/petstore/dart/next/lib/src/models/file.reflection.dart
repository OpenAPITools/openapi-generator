// Model reflection

part of 'file.dart';


//class reflection

class FileReflection extends ModelReflection<File> {
  static FileReflection instanceGetter() => instance;
  static const instance = FileReflection._(
    modelName: r'File',
    className: r'File',
    xml: XmlReflection(
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_sourceURIGetter),
      setter: FunctionWrapper2(_sourceURISetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
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
  const FileReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
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
  List<PropertyReflection<File, dynamic>> get properties => [
    sourceURIPart,
  ];

  @override
  final AdditionalPropertiesPart<File, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(File instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(File instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<File, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  File empty() {
    return File(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is FileReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



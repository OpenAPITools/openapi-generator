// Model reflection

part of 'free_form_object_test_class.dart';


//class reflection

class FreeFormObjectTestClassReflection extends ModelReflection<FreeFormObjectTestClass> {
  static FreeFormObjectTestClassReflection instanceGetter() => instance;
  static const instance = FreeFormObjectTestClassReflection._(
    modelName: r'FreeFormObjectTestClass',
    className: r'FreeFormObjectTestClass',
    xml: XmlReflection(
),
    namePart: PropertyReflection<FreeFormObjectTestClass, UndefinedWrapper<
            String
>>(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_nameGetter),
      setter: FunctionWrapper2(_nameSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    propertiesPart: PropertyReflection<FreeFormObjectTestClass, UndefinedWrapper<
            FreeFormObjectTestClassProperties
>>(
      dartName: r'properties',
      nullable: false,
      required: false,
      oasName: r'properties',
      oasType: r'FreeFormObjectTestClassProperties',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_propertiesGetter),
      setter: FunctionWrapper2(_propertiesSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                FreeFormObjectTestClassProperties.$reflection
        
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
  const FreeFormObjectTestClassReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.namePart,
    required this.propertiesPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<FreeFormObjectTestClass, UndefinedWrapper<
            String
>> namePart;
  static UndefinedWrapper<
            String
> _nameGetter(FreeFormObjectTestClass parent) {
    return parent.name;
  }
  static void _nameSetter(FreeFormObjectTestClass parent, UndefinedWrapper<
            String
> value) {
    parent.name = value;
  }

  final PropertyReflection<FreeFormObjectTestClass, UndefinedWrapper<
            FreeFormObjectTestClassProperties
>> propertiesPart;
  static UndefinedWrapper<
            FreeFormObjectTestClassProperties
> _propertiesGetter(FreeFormObjectTestClass parent) {
    return parent.properties;
  }
  static void _propertiesSetter(FreeFormObjectTestClass parent, UndefinedWrapper<
            FreeFormObjectTestClassProperties
> value) {
    parent.properties = value;
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
  List<PropertyReflection<FreeFormObjectTestClass, dynamic>> get properties => [
    namePart,
propertiesPart,
  ];

  @override
  final AdditionalPropertiesPart<FreeFormObjectTestClass, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(FreeFormObjectTestClass instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(FreeFormObjectTestClass instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<FreeFormObjectTestClass, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  FreeFormObjectTestClass empty() {
    return FreeFormObjectTestClass(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is FreeFormObjectTestClassReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



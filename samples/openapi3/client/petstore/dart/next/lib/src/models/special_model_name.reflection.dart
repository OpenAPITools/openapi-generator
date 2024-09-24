// Model reflection

part of 'special_model_name.dart';


//class reflection

class SpecialModelNameReflection extends ModelReflection<SpecialModelName> {
  static SpecialModelNameReflection instanceGetter() => instance;
  static const instance = SpecialModelNameReflection._(
    modelName: r'_special_model.name_',
    className: r'SpecialModelName',
    xml: XmlReflection(
    xmlName: r'$special[model.name]',
),
    $specialPropertyNamePart: PropertyReflection<SpecialModelName, UndefinedWrapper<
            int
>>(
      dartName: r'$specialPropertyName',
      nullable: false,
      required: false,
      oasName: r'$special[property.name]',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_$specialPropertyNameGetter),
      setter: FunctionWrapper2(_$specialPropertyNameSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    specialModelNamePart: PropertyReflection<SpecialModelName, UndefinedWrapper<
            String
>>(
      dartName: r'specialModelName',
      nullable: false,
      required: false,
      oasName: r'_special_model.name_',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_specialModelNameGetter),
      setter: FunctionWrapper2(_specialModelNameSetter),
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
  const SpecialModelNameReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.$specialPropertyNamePart,
    required this.specialModelNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<SpecialModelName, UndefinedWrapper<
            int
>> $specialPropertyNamePart;
  static UndefinedWrapper<
            int
> _$specialPropertyNameGetter(SpecialModelName parent) {
    return parent.$specialPropertyName;
  }
  static void _$specialPropertyNameSetter(SpecialModelName parent, UndefinedWrapper<
            int
> value) {
    parent.$specialPropertyName = value;
  }

  final PropertyReflection<SpecialModelName, UndefinedWrapper<
            String
>> specialModelNamePart;
  static UndefinedWrapper<
            String
> _specialModelNameGetter(SpecialModelName parent) {
    return parent.specialModelName;
  }
  static void _specialModelNameSetter(SpecialModelName parent, UndefinedWrapper<
            String
> value) {
    parent.specialModelName = value;
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
  List<PropertyReflection<SpecialModelName, dynamic>> get properties => [
    $specialPropertyNamePart,
specialModelNamePart,
  ];

  @override
  final AdditionalPropertiesPart<SpecialModelName, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(SpecialModelName instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(SpecialModelName instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<SpecialModelName, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  SpecialModelName empty() {
    return SpecialModelName(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is SpecialModelNameReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



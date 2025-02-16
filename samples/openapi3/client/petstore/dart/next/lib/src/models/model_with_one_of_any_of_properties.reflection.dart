// Model reflection

part of 'model_with_one_of_any_of_properties.dart';


//class reflection

class ModelWithOneOfAnyOfPropertiesReflection extends ModelReflection<ModelWithOneOfAnyOfProperties> {
  static ModelWithOneOfAnyOfPropertiesReflection instanceGetter() => instance;
  static const instance = ModelWithOneOfAnyOfPropertiesReflection._(
    modelName: r'ModelWithOneOfAnyOfProperties',
    className: r'ModelWithOneOfAnyOfProperties',
    xml: XmlReflection(
),
    oneofPropPart: PropertyReflection<ModelWithOneOfAnyOfProperties, UndefinedWrapper<
            ArrayOneOf
>>(
      dartName: r'oneofProp',
      nullable: false,
      required: false,
      oasName: r'oneof_prop',
      oasType: r'ArrayOneOf',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_oneofPropGetter),
      setter: FunctionWrapper2(_oneofPropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                ArrayOneOf.$reflection
        
,
)
),
    ),
    anyofPropPart: PropertyReflection<ModelWithOneOfAnyOfProperties, UndefinedWrapper<
            ArrayAnyOf
>>(
      dartName: r'anyofProp',
      nullable: false,
      required: false,
      oasName: r'anyof_prop',
      oasType: r'ArrayAnyOf',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_anyofPropGetter),
      setter: FunctionWrapper2(_anyofPropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                ArrayAnyOf.$reflection
        
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
  const ModelWithOneOfAnyOfPropertiesReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.oneofPropPart,
    required this.anyofPropPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ModelWithOneOfAnyOfProperties, UndefinedWrapper<
            ArrayOneOf
>> oneofPropPart;
  static UndefinedWrapper<
            ArrayOneOf
> _oneofPropGetter(ModelWithOneOfAnyOfProperties parent) {
    return parent.oneofProp;
  }
  static void _oneofPropSetter(ModelWithOneOfAnyOfProperties parent, UndefinedWrapper<
            ArrayOneOf
> value) {
    parent.oneofProp = value;
  }

  final PropertyReflection<ModelWithOneOfAnyOfProperties, UndefinedWrapper<
            ArrayAnyOf
>> anyofPropPart;
  static UndefinedWrapper<
            ArrayAnyOf
> _anyofPropGetter(ModelWithOneOfAnyOfProperties parent) {
    return parent.anyofProp;
  }
  static void _anyofPropSetter(ModelWithOneOfAnyOfProperties parent, UndefinedWrapper<
            ArrayAnyOf
> value) {
    parent.anyofProp = value;
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
  List<PropertyReflection<ModelWithOneOfAnyOfProperties, dynamic>> get properties => [
    oneofPropPart,
anyofPropPart,
  ];

  @override
  final AdditionalPropertiesPart<ModelWithOneOfAnyOfProperties, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ModelWithOneOfAnyOfProperties instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ModelWithOneOfAnyOfProperties instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<ModelWithOneOfAnyOfProperties, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ModelWithOneOfAnyOfProperties empty() {
    return ModelWithOneOfAnyOfProperties(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ModelWithOneOfAnyOfPropertiesReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



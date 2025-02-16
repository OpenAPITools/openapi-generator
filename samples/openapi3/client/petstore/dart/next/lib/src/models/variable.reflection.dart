// Model reflection

part of 'variable.dart';


//class reflection

class VariableReflection extends ModelReflection<Variable> {
  static VariableReflection instanceGetter() => instance;
  static const instance = VariableReflection._(
    modelName: r'Variable',
    className: r'Variable',
    xml: XmlReflection(
),
    namePart: PropertyReflection<Variable, 
            String
>(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_nameGetter),
      setter: FunctionWrapper2(_nameSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
    ),
    valuePart: PropertyReflection<Variable, 
            Value
>(
      dartName: r'value',
      nullable: false,
      required: true,
      oasName: r'value',
      oasType: r'Value',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_valueGetter),
      setter: FunctionWrapper2(_valueSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Value.$reflection
        
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
  const VariableReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.namePart,
    required this.valuePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Variable, 
            String
> namePart;
  static 
            String
 _nameGetter(Variable parent) {
    return parent.name;
  }
  static void _nameSetter(Variable parent, 
            String
 value) {
    parent.name = value;
  }

  final PropertyReflection<Variable, 
            Value
> valuePart;
  static 
            Value
 _valueGetter(Variable parent) {
    return parent.value;
  }
  static void _valueSetter(Variable parent, 
            Value
 value) {
    parent.value = value;
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
  List<PropertyReflection<Variable, dynamic>> get properties => [
    namePart,
valuePart,
  ];

  @override
  final AdditionalPropertiesPart<Variable, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Variable instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Variable instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Variable, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Variable empty() {
    return Variable(
      name: namePart.reflection.emptyFunction(),
      value: valuePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is VariableReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



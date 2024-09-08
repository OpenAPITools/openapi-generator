// Model reflection

part of 'name.dart';


//class reflection

class NameReflection extends ModelReflection<Name> {
  static NameReflection instanceGetter() => instance;
  static const instance = NameReflection._(
    modelName: r'Name',
    className: r'Name',
    xml: XmlReflection(
    xmlName: r'Name',
),
    namePart: PropertyReflection<Name, 
            int
>(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'integer',
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
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
,
    ),
    snakeCasePart: PropertyReflection<Name, UndefinedWrapper<
            int
>>(
      dartName: r'snakeCase',
      nullable: false,
      required: false,
      oasName: r'snake_case',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_snakeCaseGetter),
      setter: FunctionWrapper2(_snakeCaseSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    propertyPart: PropertyReflection<Name, UndefinedWrapper<
            String
>>(
      dartName: r'property',
      nullable: false,
      required: false,
      oasName: r'property',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_propertyGetter),
      setter: FunctionWrapper2(_propertySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    $123numberPart: PropertyReflection<Name, UndefinedWrapper<
            int
>>(
      dartName: r'$123number',
      nullable: false,
      required: false,
      oasName: r'123Number',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_$123numberGetter),
      setter: FunctionWrapper2(_$123numberSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
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
  const NameReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.namePart,
    required this.snakeCasePart,
    required this.propertyPart,
    required this.$123numberPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Name, 
            int
> namePart;
  static 
            int
 _nameGetter(Name parent) {
    return parent.name;
  }
  static void _nameSetter(Name parent, 
            int
 value) {
    parent.name = value;
  }

  final PropertyReflection<Name, UndefinedWrapper<
            int
>> snakeCasePart;
  static UndefinedWrapper<
            int
> _snakeCaseGetter(Name parent) {
    return parent.snakeCase;
  }
  static void _snakeCaseSetter(Name parent, UndefinedWrapper<
            int
> value) {
    parent.snakeCase = value;
  }

  final PropertyReflection<Name, UndefinedWrapper<
            String
>> propertyPart;
  static UndefinedWrapper<
            String
> _propertyGetter(Name parent) {
    return parent.property;
  }
  static void _propertySetter(Name parent, UndefinedWrapper<
            String
> value) {
    parent.property = value;
  }

  final PropertyReflection<Name, UndefinedWrapper<
            int
>> $123numberPart;
  static UndefinedWrapper<
            int
> _$123numberGetter(Name parent) {
    return parent.$123number;
  }
  static void _$123numberSetter(Name parent, UndefinedWrapper<
            int
> value) {
    parent.$123number = value;
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
  List<PropertyReflection<Name, dynamic>> get properties => [
    namePart,
snakeCasePart,
propertyPart,
$123numberPart,
  ];

  @override
  final AdditionalPropertiesPart<Name, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Name instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Name instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Name, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Name empty() {
    return Name(
      name: namePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is NameReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



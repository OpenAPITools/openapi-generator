// Model reflection

part of 'read_only_first.dart';


//class reflection

class ReadOnlyFirstReflection extends ModelReflection<ReadOnlyFirst> {
  static ReadOnlyFirstReflection instanceGetter() => instance;
  static const instance = ReadOnlyFirstReflection._(
    modelName: r'ReadOnlyFirst',
    className: r'ReadOnlyFirst',
    xml: XmlReflection(
),
    barPart: PropertyReflection<ReadOnlyFirst, UndefinedWrapper<
            String
>>(
      dartName: r'bar',
      nullable: false,
      required: false,
      oasName: r'bar',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_barGetter),
      setter: FunctionWrapper2(_barSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    bazPart: PropertyReflection<ReadOnlyFirst, UndefinedWrapper<
            String
>>(
      dartName: r'baz',
      nullable: false,
      required: false,
      oasName: r'baz',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_bazGetter),
      setter: FunctionWrapper2(_bazSetter),
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
  const ReadOnlyFirstReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.barPart,
    required this.bazPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ReadOnlyFirst, UndefinedWrapper<
            String
>> barPart;
  static UndefinedWrapper<
            String
> _barGetter(ReadOnlyFirst parent) {
    return parent.bar;
  }
  static void _barSetter(ReadOnlyFirst parent, UndefinedWrapper<
            String
> value) {
    parent.bar = value;
  }

  final PropertyReflection<ReadOnlyFirst, UndefinedWrapper<
            String
>> bazPart;
  static UndefinedWrapper<
            String
> _bazGetter(ReadOnlyFirst parent) {
    return parent.baz;
  }
  static void _bazSetter(ReadOnlyFirst parent, UndefinedWrapper<
            String
> value) {
    parent.baz = value;
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
  List<PropertyReflection<ReadOnlyFirst, dynamic>> get properties => [
    barPart,
bazPart,
  ];

  @override
  final AdditionalPropertiesPart<ReadOnlyFirst, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ReadOnlyFirst instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ReadOnlyFirst instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<ReadOnlyFirst, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ReadOnlyFirst empty() {
    return ReadOnlyFirst(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ReadOnlyFirstReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



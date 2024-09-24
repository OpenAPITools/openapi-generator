// Model reflection

part of 'has_only_read_only.dart';


//class reflection

class HasOnlyReadOnlyReflection extends ModelReflection<HasOnlyReadOnly> {
  static HasOnlyReadOnlyReflection instanceGetter() => instance;
  static const instance = HasOnlyReadOnlyReflection._(
    modelName: r'hasOnlyReadOnly',
    className: r'HasOnlyReadOnly',
    xml: XmlReflection(
),
    barPart: PropertyReflection<HasOnlyReadOnly, UndefinedWrapper<
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
    fooPart: PropertyReflection<HasOnlyReadOnly, UndefinedWrapper<
            String
>>(
      dartName: r'foo',
      nullable: false,
      required: false,
      oasName: r'foo',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_fooGetter),
      setter: FunctionWrapper2(_fooSetter),
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
  const HasOnlyReadOnlyReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.barPart,
    required this.fooPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<HasOnlyReadOnly, UndefinedWrapper<
            String
>> barPart;
  static UndefinedWrapper<
            String
> _barGetter(HasOnlyReadOnly parent) {
    return parent.bar;
  }
  static void _barSetter(HasOnlyReadOnly parent, UndefinedWrapper<
            String
> value) {
    parent.bar = value;
  }

  final PropertyReflection<HasOnlyReadOnly, UndefinedWrapper<
            String
>> fooPart;
  static UndefinedWrapper<
            String
> _fooGetter(HasOnlyReadOnly parent) {
    return parent.foo;
  }
  static void _fooSetter(HasOnlyReadOnly parent, UndefinedWrapper<
            String
> value) {
    parent.foo = value;
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
  List<PropertyReflection<HasOnlyReadOnly, dynamic>> get properties => [
    barPart,
fooPart,
  ];

  @override
  final AdditionalPropertiesPart<HasOnlyReadOnly, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(HasOnlyReadOnly instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(HasOnlyReadOnly instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<HasOnlyReadOnly, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  HasOnlyReadOnly empty() {
    return HasOnlyReadOnly(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is HasOnlyReadOnlyReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



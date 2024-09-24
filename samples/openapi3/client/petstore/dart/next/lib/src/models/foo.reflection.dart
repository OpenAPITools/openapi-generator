// Model reflection

part of 'foo.dart';


//class reflection

class FooReflection extends ModelReflection<Foo> {
  static FooReflection instanceGetter() => instance;
  static const instance = FooReflection._(
    modelName: r'Foo',
    className: r'Foo',
    xml: XmlReflection(
),
    barPart: PropertyReflection<Foo, UndefinedWrapper<
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
  const FooReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.barPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Foo, UndefinedWrapper<
            String
>> barPart;
  static UndefinedWrapper<
            String
> _barGetter(Foo parent) {
    return parent.bar;
  }
  static void _barSetter(Foo parent, UndefinedWrapper<
            String
> value) {
    parent.bar = value;
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
  List<PropertyReflection<Foo, dynamic>> get properties => [
    barPart,
  ];

  @override
  final AdditionalPropertiesPart<Foo, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Foo instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Foo instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Foo, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Foo empty() {
    return Foo(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is FooReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



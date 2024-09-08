// Model reflection

part of 'foo_get_default_response.dart';


//class reflection

class FooGetDefaultResponseReflection extends ModelReflection<FooGetDefaultResponse> {
  static FooGetDefaultResponseReflection instanceGetter() => instance;
  static const instance = FooGetDefaultResponseReflection._(
    modelName: r'_foo_get_default_response',
    className: r'FooGetDefaultResponse',
    xml: XmlReflection(
),
    stringPart: PropertyReflection<FooGetDefaultResponse, UndefinedWrapper<
            Foo
>>(
      dartName: r'string',
      nullable: false,
      required: false,
      oasName: r'string',
      oasType: r'Foo',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_stringGetter),
      setter: FunctionWrapper2(_stringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Foo.$reflection
        
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
  const FooGetDefaultResponseReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.stringPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<FooGetDefaultResponse, UndefinedWrapper<
            Foo
>> stringPart;
  static UndefinedWrapper<
            Foo
> _stringGetter(FooGetDefaultResponse parent) {
    return parent.string;
  }
  static void _stringSetter(FooGetDefaultResponse parent, UndefinedWrapper<
            Foo
> value) {
    parent.string = value;
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
  List<PropertyReflection<FooGetDefaultResponse, dynamic>> get properties => [
    stringPart,
  ];

  @override
  final AdditionalPropertiesPart<FooGetDefaultResponse, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(FooGetDefaultResponse instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(FooGetDefaultResponse instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<FooGetDefaultResponse, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  FooGetDefaultResponse empty() {
    return FooGetDefaultResponse(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is FooGetDefaultResponseReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



// Model reflection

part of '__return.dart';


//class reflection

class $ReturnReflection extends ModelReflection<$Return> {
  static $ReturnReflection instanceGetter() => instance;
  static const instance = $ReturnReflection._(
    modelName: r'Return',
    className: r'$Return',
    xml: XmlReflection(
    xmlName: r'Return',
),
    $returnPart: PropertyReflection<$Return, UndefinedWrapper<
            int
>>(
      dartName: r'$return',
      nullable: false,
      required: false,
      oasName: r'return',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_$returnGetter),
      setter: FunctionWrapper2(_$returnSetter),
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
  const $ReturnReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.$returnPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<$Return, UndefinedWrapper<
            int
>> $returnPart;
  static UndefinedWrapper<
            int
> _$returnGetter($Return parent) {
    return parent.$return;
  }
  static void _$returnSetter($Return parent, UndefinedWrapper<
            int
> value) {
    parent.$return = value;
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
  List<PropertyReflection<$Return, dynamic>> get properties => [
    $returnPart,
  ];

  @override
  final AdditionalPropertiesPart<$Return, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter($Return instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter($Return instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<$Return, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  $Return empty() {
    return $Return(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is $ReturnReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



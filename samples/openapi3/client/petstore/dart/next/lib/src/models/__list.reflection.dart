// Model reflection

part of '__list.dart';


//class reflection

class $ListReflection extends ModelReflection<$List> {
  static $ListReflection instanceGetter() => instance;
  static const instance = $ListReflection._(
    modelName: r'List',
    className: r'$List',
    xml: XmlReflection(
),
    $123listPart: PropertyReflection<$List, UndefinedWrapper<
            String
>>(
      dartName: r'$123list',
      nullable: false,
      required: false,
      oasName: r'123-list',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_$123listGetter),
      setter: FunctionWrapper2(_$123listSetter),
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
  const $ListReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.$123listPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<$List, UndefinedWrapper<
            String
>> $123listPart;
  static UndefinedWrapper<
            String
> _$123listGetter($List parent) {
    return parent.$123list;
  }
  static void _$123listSetter($List parent, UndefinedWrapper<
            String
> value) {
    parent.$123list = value;
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
  List<PropertyReflection<$List, dynamic>> get properties => [
    $123listPart,
  ];

  @override
  final AdditionalPropertiesPart<$List, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter($List instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter($List instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<$List, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  $List empty() {
    return $List(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is $ListReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



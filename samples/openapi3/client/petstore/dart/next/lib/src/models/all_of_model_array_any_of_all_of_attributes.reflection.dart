// Model reflection

part of 'all_of_model_array_any_of_all_of_attributes.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfAttributesReflection extends ModelReflection<AllOfModelArrayAnyOfAllOfAttributes> {
  static AllOfModelArrayAnyOfAllOfAttributesReflection instanceGetter() => instance;
  static const instance = AllOfModelArrayAnyOfAllOfAttributesReflection._(
    modelName: r'AllOfModelArrayAnyOf_allOf_attributes',
    className: r'AllOfModelArrayAnyOfAllOfAttributes',
    xml: XmlReflection(
),
    CPart: PropertyReflection<AllOfModelArrayAnyOfAllOfAttributes, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC
>>(
      dartName: r'C',
      nullable: false,
      required: false,
      oasName: r'C',
      oasType: r'AllOfModelArrayAnyOfAllOfAttributesC',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_CGetter),
      setter: FunctionWrapper2(_CSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                AllOfModelArrayAnyOfAllOfAttributesC.$reflection
        
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
  const AllOfModelArrayAnyOfAllOfAttributesReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.CPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<AllOfModelArrayAnyOfAllOfAttributes, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC
>> CPart;
  static UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC
> _CGetter(AllOfModelArrayAnyOfAllOfAttributes parent) {
    return parent.C;
  }
  static void _CSetter(AllOfModelArrayAnyOfAllOfAttributes parent, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC
> value) {
    parent.C = value;
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
  List<PropertyReflection<AllOfModelArrayAnyOfAllOfAttributes, dynamic>> get properties => [
    CPart,
  ];

  @override
  final AdditionalPropertiesPart<AllOfModelArrayAnyOfAllOfAttributes, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(AllOfModelArrayAnyOfAllOfAttributes instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(AllOfModelArrayAnyOfAllOfAttributes instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<AllOfModelArrayAnyOfAllOfAttributes, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  AllOfModelArrayAnyOfAllOfAttributes empty() {
    return AllOfModelArrayAnyOfAllOfAttributes(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is AllOfModelArrayAnyOfAllOfAttributesReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



// Model reflection

part of 'all_of_model_array_any_of_all_of_link_list_column1.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection extends ModelReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1> {
  static AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection instanceGetter() => instance;
  static const instance = AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection._(
    modelName: r'AllOfModelArrayAnyOf_allOf_linkListColumn1',
    className: r'AllOfModelArrayAnyOfAllOfLinkListColumn1',
    xml: XmlReflection(
),
    valuePart: PropertyReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1, 
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value
>
>(
      dartName: r'value',
      nullable: false,
      required: true,
      oasName: r'value',
      oasType: r'array',
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
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                AllOfModelArrayAnyOfAllOfLinkListColumn1Value.$reflection
        
,
)
)
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
  const AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.valuePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1, 
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value
>
> valuePart;
  static 
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value
>
 _valueGetter(AllOfModelArrayAnyOfAllOfLinkListColumn1 parent) {
    return parent.value;
  }
  static void _valueSetter(AllOfModelArrayAnyOfAllOfLinkListColumn1 parent, 
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value
>
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
  List<PropertyReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1, dynamic>> get properties => [
    valuePart,
  ];

  @override
  final AdditionalPropertiesPart<AllOfModelArrayAnyOfAllOfLinkListColumn1, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(AllOfModelArrayAnyOfAllOfLinkListColumn1 instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(AllOfModelArrayAnyOfAllOfLinkListColumn1 instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  AllOfModelArrayAnyOfAllOfLinkListColumn1 empty() {
    return AllOfModelArrayAnyOfAllOfLinkListColumn1(
      value: valuePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



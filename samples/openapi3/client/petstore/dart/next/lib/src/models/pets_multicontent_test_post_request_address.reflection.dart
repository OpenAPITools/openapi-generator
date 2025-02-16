// Model reflection

part of 'pets_multicontent_test_post_request_address.dart';


//class reflection

class PetsMulticontentTestPostRequestAddressReflection extends ModelReflection<PetsMulticontentTestPostRequestAddress> {
  static PetsMulticontentTestPostRequestAddressReflection instanceGetter() => instance;
  static const instance = PetsMulticontentTestPostRequestAddressReflection._(
    modelName: r'_pets_multicontent_test_post_request_address',
    className: r'PetsMulticontentTestPostRequestAddress',
    xml: XmlReflection(
),
    streetPart: PropertyReflection<PetsMulticontentTestPostRequestAddress, UndefinedWrapper<
            String
>>(
      dartName: r'street',
      nullable: false,
      required: false,
      oasName: r'street',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_streetGetter),
      setter: FunctionWrapper2(_streetSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    cityPart: PropertyReflection<PetsMulticontentTestPostRequestAddress, UndefinedWrapper<
            String
>>(
      dartName: r'city',
      nullable: false,
      required: false,
      oasName: r'city',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_cityGetter),
      setter: FunctionWrapper2(_citySetter),
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
  const PetsMulticontentTestPostRequestAddressReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.streetPart,
    required this.cityPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<PetsMulticontentTestPostRequestAddress, UndefinedWrapper<
            String
>> streetPart;
  static UndefinedWrapper<
            String
> _streetGetter(PetsMulticontentTestPostRequestAddress parent) {
    return parent.street;
  }
  static void _streetSetter(PetsMulticontentTestPostRequestAddress parent, UndefinedWrapper<
            String
> value) {
    parent.street = value;
  }

  final PropertyReflection<PetsMulticontentTestPostRequestAddress, UndefinedWrapper<
            String
>> cityPart;
  static UndefinedWrapper<
            String
> _cityGetter(PetsMulticontentTestPostRequestAddress parent) {
    return parent.city;
  }
  static void _citySetter(PetsMulticontentTestPostRequestAddress parent, UndefinedWrapper<
            String
> value) {
    parent.city = value;
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
  List<PropertyReflection<PetsMulticontentTestPostRequestAddress, dynamic>> get properties => [
    streetPart,
cityPart,
  ];

  @override
  final AdditionalPropertiesPart<PetsMulticontentTestPostRequestAddress, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(PetsMulticontentTestPostRequestAddress instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(PetsMulticontentTestPostRequestAddress instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<PetsMulticontentTestPostRequestAddress, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  PetsMulticontentTestPostRequestAddress empty() {
    return PetsMulticontentTestPostRequestAddress(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is PetsMulticontentTestPostRequestAddressReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



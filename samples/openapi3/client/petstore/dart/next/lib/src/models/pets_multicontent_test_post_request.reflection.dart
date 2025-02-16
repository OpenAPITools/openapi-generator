// Model reflection

part of 'pets_multicontent_test_post_request.dart';


//class reflection

class PetsMulticontentTestPostRequestReflection extends ModelReflection<PetsMulticontentTestPostRequest> {
  static PetsMulticontentTestPostRequestReflection instanceGetter() => instance;
  static const instance = PetsMulticontentTestPostRequestReflection._(
    modelName: r'_pets_multicontent_test_post_request',
    className: r'PetsMulticontentTestPostRequest',
    xml: XmlReflection(
),
    idPart: PropertyReflection<PetsMulticontentTestPostRequest, UndefinedWrapper<
            String
>>(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_idGetter),
      setter: FunctionWrapper2(_idSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    addressPart: PropertyReflection<PetsMulticontentTestPostRequest, UndefinedWrapper<
            PetsMulticontentTestPostRequestAddress
>>(
      dartName: r'address',
      nullable: false,
      required: false,
      oasName: r'address',
      oasType: r'PetsMulticontentTestPostRequestAddress',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_addressGetter),
      setter: FunctionWrapper2(_addressSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PetsMulticontentTestPostRequestAddress.$reflection
        
,
)
),
    ),
    profileImagesPart: PropertyReflection<PetsMulticontentTestPostRequest, UndefinedWrapper<
    List<
        
            XFile
>
>>(
      dartName: r'profileImages',
      nullable: false,
      required: false,
      oasName: r'profileImages',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_profileImagesGetter),
      setter: FunctionWrapper2(_profileImagesSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forXFile
        
,
)
)
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
  const PetsMulticontentTestPostRequestReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.idPart,
    required this.addressPart,
    required this.profileImagesPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<PetsMulticontentTestPostRequest, UndefinedWrapper<
            String
>> idPart;
  static UndefinedWrapper<
            String
> _idGetter(PetsMulticontentTestPostRequest parent) {
    return parent.id;
  }
  static void _idSetter(PetsMulticontentTestPostRequest parent, UndefinedWrapper<
            String
> value) {
    parent.id = value;
  }

  final PropertyReflection<PetsMulticontentTestPostRequest, UndefinedWrapper<
            PetsMulticontentTestPostRequestAddress
>> addressPart;
  static UndefinedWrapper<
            PetsMulticontentTestPostRequestAddress
> _addressGetter(PetsMulticontentTestPostRequest parent) {
    return parent.address;
  }
  static void _addressSetter(PetsMulticontentTestPostRequest parent, UndefinedWrapper<
            PetsMulticontentTestPostRequestAddress
> value) {
    parent.address = value;
  }

  final PropertyReflection<PetsMulticontentTestPostRequest, UndefinedWrapper<
    List<
        
            XFile
>
>> profileImagesPart;
  static UndefinedWrapper<
    List<
        
            XFile
>
> _profileImagesGetter(PetsMulticontentTestPostRequest parent) {
    return parent.profileImages;
  }
  static void _profileImagesSetter(PetsMulticontentTestPostRequest parent, UndefinedWrapper<
    List<
        
            XFile
>
> value) {
    parent.profileImages = value;
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
  List<PropertyReflection<PetsMulticontentTestPostRequest, dynamic>> get properties => [
    idPart,
addressPart,
profileImagesPart,
  ];

  @override
  final AdditionalPropertiesPart<PetsMulticontentTestPostRequest, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(PetsMulticontentTestPostRequest instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(PetsMulticontentTestPostRequest instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<PetsMulticontentTestPostRequest, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  PetsMulticontentTestPostRequest empty() {
    return PetsMulticontentTestPostRequest(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is PetsMulticontentTestPostRequestReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



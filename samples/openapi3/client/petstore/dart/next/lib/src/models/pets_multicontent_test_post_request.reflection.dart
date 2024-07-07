// Model reflection

part of 'pets_multicontent_test_post_request.dart';


//class reflection

class PetsMulticontentTestPostRequestReflection extends ClassReflection<PetsMulticontentTestPostRequest> {
  static PetsMulticontentTestPostRequestReflection instanceGetter() => instance;
  static const instance = PetsMulticontentTestPostRequestReflection._(
    modelName: r'_pets_multicontent_test_post_request',
    className: r'PetsMulticontentTestPostRequest',
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
      getter: _idGetter,
      setter: _idSetter,
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
      classReflection: PetsMulticontentTestPostRequestAddressReflection.instance,
      getter: _addressGetter,
      setter: _addressSetter,
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
      itemsReflection: ItemsReflection<PetsMulticontentTestPostRequest, 
            XFile
>(parentReflectionGetter: instanceGetter,),
      getter: _profileImagesGetter,
      setter: _profileImagesSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<PetsMulticontentTestPostRequest, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const PetsMulticontentTestPostRequestReflection._({
    required this.modelName,
    required this.className,
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
  final Map<String, ClassReflection> discriminatorMappings;
  @override
  final Map<String, ClassReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;


  @override
  List<PropertyReflection<PetsMulticontentTestPostRequest, dynamic>> get properties => [
    idPart,
addressPart,
profileImagesPart,
  ];

  final AdditionalPropertiesReflection<PetsMulticontentTestPostRequest, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<PetsMulticontentTestPostRequest, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<PetsMulticontentTestPostRequest, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => PetsMulticontentTestPostRequest.canDeserialize(src);
  @override
  PetsMulticontentTestPostRequest Function(Object? src) get deserializeFunction =>
      (src) => PetsMulticontentTestPostRequest.deserialize(src);

  @override
  Object? Function(PetsMulticontentTestPostRequest src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of PetsMulticontentTestPostRequest.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  PetsMulticontentTestPostRequest example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return PetsMulticontentTestPostRequest(
      id: () {
        PartReflection? _partReflection = _reflection.idPart;
        
        final disc = discriminators[r'id'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return UndefinedWrapper(result);
          }
        }
        
        return UndefinedWrapper(


            
            


    
    exampleString()


);
      }(),
      address: () {
        PartReflection? _partReflection = _reflection.addressPart;
        
        return UndefinedWrapper(


            
            


    PetsMulticontentTestPostRequestAddress.$reflection.example()
    


);
      }(),
      profileImages: () {
        PartReflection? _partReflection = _reflection.profileImagesPart;
        
        return UndefinedWrapper(


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleXFile()


; })



);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class PetsMulticontentTestPostRequestXmlReflection {
    const PetsMulticontentTestPostRequestXmlReflection();
}


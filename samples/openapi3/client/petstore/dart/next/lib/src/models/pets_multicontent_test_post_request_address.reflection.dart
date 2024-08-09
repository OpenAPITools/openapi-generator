// Model reflection

part of 'pets_multicontent_test_post_request_address.dart';


//class reflection

class PetsMulticontentTestPostRequestAddressReflection extends ClassReflection<PetsMulticontentTestPostRequestAddress> {
  static PetsMulticontentTestPostRequestAddressReflection instanceGetter() => instance;
  static const instance = PetsMulticontentTestPostRequestAddressReflection._(
    modelName: r'_pets_multicontent_test_post_request_address',
    className: r'PetsMulticontentTestPostRequestAddress',
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
      getter: _streetGetter,
      setter: _streetSetter,
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
      getter: _cityGetter,
      setter: _citySetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<PetsMulticontentTestPostRequestAddress, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const PetsMulticontentTestPostRequestAddressReflection._({
    required this.modelName,
    required this.className,
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
  List<PropertyReflection<PetsMulticontentTestPostRequestAddress, dynamic>> get properties => [
    streetPart,
cityPart,
  ];

  final AdditionalPropertiesReflection<PetsMulticontentTestPostRequestAddress, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<PetsMulticontentTestPostRequestAddress, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<PetsMulticontentTestPostRequestAddress, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => PetsMulticontentTestPostRequestAddress.canDeserialize(src);
  @override
  PetsMulticontentTestPostRequestAddress Function(Object? src) get deserializeFunction =>
      (src) => PetsMulticontentTestPostRequestAddress.deserialize(src);

  @override
  Object? Function(PetsMulticontentTestPostRequestAddress src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of PetsMulticontentTestPostRequestAddress.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  PetsMulticontentTestPostRequestAddress example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
        discriminatorExampleResults = const {},}) {
    final _reflection = this;
    final actualDiscriminators = discriminators ?? _reflection.aggregatedDiscriminators;
    discriminatorExampleResults = Map.from(discriminatorExampleResults);
    for (final MapEntry(key: propName, value: mappings) in actualDiscriminators.entries) {
      if (discriminatorExampleResults.containsKey(propName)) {
        continue;
      }
      final r =  exampleDiscriminator(mappings);
      if (r != null){
        discriminatorExampleResults[propName] = r;
      }
    }

    final exampleResult = PetsMulticontentTestPostRequestAddress(
      street: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[streetPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      city: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[cityPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class PetsMulticontentTestPostRequestAddressXmlReflection {
    const PetsMulticontentTestPostRequestAddressXmlReflection();
}


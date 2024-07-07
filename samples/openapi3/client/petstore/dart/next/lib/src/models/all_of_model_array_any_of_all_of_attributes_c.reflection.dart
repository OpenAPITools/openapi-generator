// Model reflection

part of 'all_of_model_array_any_of_all_of_attributes_c.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfAttributesCReflection extends ClassReflection<AllOfModelArrayAnyOfAllOfAttributesC> {
  static AllOfModelArrayAnyOfAllOfAttributesCReflection instanceGetter() => instance;
  static const instance = AllOfModelArrayAnyOfAllOfAttributesCReflection._(
    modelName: r'AllOfModelArrayAnyOf_allOf_attributes_C',
    className: r'AllOfModelArrayAnyOfAllOfAttributesC',
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: PetReflection.instance,
    ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: OrderReflection.instance,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<AllOfModelArrayAnyOfAllOfAttributesC, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const AllOfModelArrayAnyOfAllOfAttributesCReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.additionalPropertiesPart,
  });




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
  List<PropertyReflection<AllOfModelArrayAnyOfAllOfAttributesC, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<AllOfModelArrayAnyOfAllOfAttributesC, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, 
            Pet
> oneOf0Part;
  
  final OneOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, 
            Order
> oneOf1Part;
  
  @override
  List<PartReflection<AllOfModelArrayAnyOfAllOfAttributesC, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AllOfModelArrayAnyOfAllOfAttributesC.canDeserialize(src);
  @override
  AllOfModelArrayAnyOfAllOfAttributesC Function(Object? src) get deserializeFunction =>
      (src) => AllOfModelArrayAnyOfAllOfAttributesC.deserialize(src);

  @override
  Object? Function(AllOfModelArrayAnyOfAllOfAttributesC src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of AllOfModelArrayAnyOfAllOfAttributesC.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  AllOfModelArrayAnyOfAllOfAttributesC example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return AllOfModelArrayAnyOfAllOfAttributesC(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      oneOf0: () {
        PartReflection? _partReflection = _reflection.oneOf0Part;
        return UndefinedWrapper(


            
            


    Pet.$reflection.example(discriminators: discriminators)
    


);
      }(),
      
    );
  }
}

class AllOfModelArrayAnyOfAllOfAttributesCXmlReflection {
    const AllOfModelArrayAnyOfAllOfAttributesCXmlReflection();
}


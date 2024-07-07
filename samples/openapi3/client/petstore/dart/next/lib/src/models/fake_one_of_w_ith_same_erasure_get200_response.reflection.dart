// Model reflection

part of 'fake_one_of_w_ith_same_erasure_get200_response.dart';


//class reflection

class FakeOneOfWIthSameErasureGet200ResponseReflection extends ClassReflection<FakeOneOfWIthSameErasureGet200Response> {
  static FakeOneOfWIthSameErasureGet200ResponseReflection instanceGetter() => instance;
  static const instance = FakeOneOfWIthSameErasureGet200ResponseReflection._(
    modelName: r'_fake_oneOfWIthSameErasure_get_200_response',
    className: r'FakeOneOfWIthSameErasureGet200Response',
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FakeOneOfWIthSameErasureGet200Response, 
            String
>(parentReflectionGetter: instanceGetter,),
          ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FakeOneOfWIthSameErasureGet200Response, 
            int
>(parentReflectionGetter: instanceGetter,),
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FakeOneOfWIthSameErasureGet200Response, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const FakeOneOfWIthSameErasureGet200ResponseReflection._({
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
  List<PropertyReflection<FakeOneOfWIthSameErasureGet200Response, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<FakeOneOfWIthSameErasureGet200Response, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<FakeOneOfWIthSameErasureGet200Response, 
    List<
        
            String
>
> oneOf0Part;
  
  final OneOfReflection<FakeOneOfWIthSameErasureGet200Response, 
    List<
        
            int
>
> oneOf1Part;
  
  @override
  List<PartReflection<FakeOneOfWIthSameErasureGet200Response, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<FakeOneOfWIthSameErasureGet200Response, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<FakeOneOfWIthSameErasureGet200Response, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<FakeOneOfWIthSameErasureGet200Response, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FakeOneOfWIthSameErasureGet200Response.canDeserialize(src);
  @override
  FakeOneOfWIthSameErasureGet200Response Function(Object? src) get deserializeFunction =>
      (src) => FakeOneOfWIthSameErasureGet200Response.deserialize(src);

  @override
  Object? Function(FakeOneOfWIthSameErasureGet200Response src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of FakeOneOfWIthSameErasureGet200Response.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  FakeOneOfWIthSameErasureGet200Response example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return FakeOneOfWIthSameErasureGet200Response(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      oneOf0: () {
        PartReflection? _partReflection = _reflection.oneOf0Part;
        return UndefinedWrapper(


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



);
      }(),
      
    );
  }
}

class FakeOneOfWIthSameErasureGet200ResponseXmlReflection {
    const FakeOneOfWIthSameErasureGet200ResponseXmlReflection();
}


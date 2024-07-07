// Model reflection

part of 'fake_any_of_w_ith_same_erasure_get200_response.dart';


//class reflection

class FakeAnyOfWIthSameErasureGet200ResponseReflection extends ClassReflection<FakeAnyOfWIthSameErasureGet200Response> {
  static FakeAnyOfWIthSameErasureGet200ResponseReflection instanceGetter() => instance;
  static const instance = FakeAnyOfWIthSameErasureGet200ResponseReflection._(
    modelName: r'_fake_anyOfWIthSameErasure_get_200_response',
    className: r'FakeAnyOfWIthSameErasureGet200Response',
    
    
    anyOf0Part: AnyOfReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FakeAnyOfWIthSameErasureGet200Response, 
            String
>(parentReflectionGetter: instanceGetter,),
          ),
    
    anyOf1Part: AnyOfReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FakeAnyOfWIthSameErasureGet200Response, 
            int
>(parentReflectionGetter: instanceGetter,),
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FakeAnyOfWIthSameErasureGet200Response, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const FakeAnyOfWIthSameErasureGet200ResponseReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.anyOf0Part,
    
    required this.anyOf1Part,
    
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
  List<PropertyReflection<FakeAnyOfWIthSameErasureGet200Response, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<FakeAnyOfWIthSameErasureGet200Response, Object
?> additionalPropertiesPart;

  
  
  final AnyOfReflection<FakeAnyOfWIthSameErasureGet200Response, 
    List<
        
            String
>
> anyOf0Part;
  
  final AnyOfReflection<FakeAnyOfWIthSameErasureGet200Response, 
    List<
        
            int
>
> anyOf1Part;
  
  @override
  List<PartReflection<FakeAnyOfWIthSameErasureGet200Response, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<FakeAnyOfWIthSameErasureGet200Response, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<FakeAnyOfWIthSameErasureGet200Response, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<FakeAnyOfWIthSameErasureGet200Response, dynamic>> get anyOfs => [
    anyOf0Part,anyOf1Part,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FakeAnyOfWIthSameErasureGet200Response.canDeserialize(src);
  @override
  FakeAnyOfWIthSameErasureGet200Response Function(Object? src) get deserializeFunction =>
      (src) => FakeAnyOfWIthSameErasureGet200Response.deserialize(src);

  @override
  Object? Function(FakeAnyOfWIthSameErasureGet200Response src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of FakeAnyOfWIthSameErasureGet200Response.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  FakeAnyOfWIthSameErasureGet200Response example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return FakeAnyOfWIthSameErasureGet200Response(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      anyOf0: () {
        PartReflection? _partReflection = _reflection.anyOf0Part;
        return UndefinedWrapper(


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



);
      }(),
      anyOf1: () {
        PartReflection? _partReflection = _reflection.anyOf1Part;
        return UndefinedWrapper(


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleint()


; })



);
      }(),
    );
  }
}

class FakeAnyOfWIthSameErasureGet200ResponseXmlReflection {
    const FakeAnyOfWIthSameErasureGet200ResponseXmlReflection();
}


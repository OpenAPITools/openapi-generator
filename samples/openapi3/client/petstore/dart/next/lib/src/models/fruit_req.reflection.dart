// Model reflection

part of 'fruit_req.dart';


//class reflection

class FruitReqReflection extends ClassReflection<FruitReq> {
  static FruitReqReflection instanceGetter() => instance;
  static const instance = FruitReqReflection._(
    modelName: r'fruitReq',
    className: r'FruitReq',
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: AppleReqReflection.instance,
    ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: BananaReqReflection.instance,
    ),
    
  );
  const FruitReqReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
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
  List<PropertyReflection<FruitReq, dynamic>> get properties => [
      ];

  
  
  
  final OneOfReflection<FruitReq, 
            AppleReq
> oneOf0Part;
  
  final OneOfReflection<FruitReq, 
            BananaReq
> oneOf1Part;
  
  @override
  List<PartReflection<FruitReq, dynamic>> get parts => [
    ...super.parts,
      ];
  @override
  List<AllOfReflection<FruitReq, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<FruitReq, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<FruitReq, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FruitReq.canDeserialize(src);
  @override
  FruitReq Function(Object? src) get deserializeFunction =>
      (src) => FruitReq.deserialize(src);

  @override
  Object? Function(FruitReq src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of FruitReq.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  FruitReq example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return FruitReq(
      
      
      oneOf0: () {
        PartReflection? _partReflection = _reflection.oneOf0Part;
        return UndefinedWrapper(


            
            


    AppleReq.$reflection.example(discriminators: discriminators)
    


);
      }(),
      
    );
  }
}

class FruitReqXmlReflection {
    const FruitReqXmlReflection();
}


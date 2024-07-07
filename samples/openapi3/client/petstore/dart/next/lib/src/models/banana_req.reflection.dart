// Model reflection

part of 'banana_req.dart';


//class reflection

class BananaReqReflection extends ClassReflection<BananaReq> {
  static BananaReqReflection instanceGetter() => instance;
  static const instance = BananaReqReflection._(
    modelName: r'bananaReq',
    className: r'BananaReq',
    lengthCmPart: PropertyReflection<BananaReq, 
            num
>(
      dartName: r'lengthCm',
      nullable: false,
      required: true,
      oasName: r'lengthCm',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _lengthCmGetter,
      setter: _lengthCmSetter,
    ),
    sweetPart: PropertyReflection<BananaReq, UndefinedWrapper<
            bool
>>(
      dartName: r'sweet',
      nullable: false,
      required: false,
      oasName: r'sweet',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _sweetGetter,
      setter: _sweetSetter,
    ),
    
    
  );
  const BananaReqReflection._({
    required this.modelName,
    required this.className,
    required this.lengthCmPart,
    required this.sweetPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
  });

  final PropertyReflection<BananaReq, 
            num
> lengthCmPart;
  static 
            num
 _lengthCmGetter(BananaReq parent) {
    return parent.lengthCm;
  }
  static void _lengthCmSetter(BananaReq parent, 
            num
 value) {
    parent.lengthCm = value;
  }
  final PropertyReflection<BananaReq, UndefinedWrapper<
            bool
>> sweetPart;
  static UndefinedWrapper<
            bool
> _sweetGetter(BananaReq parent) {
    return parent.sweet;
  }
  static void _sweetSetter(BananaReq parent, UndefinedWrapper<
            bool
> value) {
    parent.sweet = value;
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
  List<PropertyReflection<BananaReq, dynamic>> get properties => [
    lengthCmPart,
sweetPart,
  ];

  
  
  
  @override
  List<PartReflection<BananaReq, dynamic>> get parts => [
    ...super.parts,
      ];
  @override
  List<AllOfReflection<BananaReq, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => BananaReq.canDeserialize(src);
  @override
  BananaReq Function(Object? src) get deserializeFunction =>
      (src) => BananaReq.deserialize(src);

  @override
  Object? Function(BananaReq src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of BananaReq.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  BananaReq example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return BananaReq(
      lengthCm: () {
        PartReflection? _partReflection = _reflection.lengthCmPart;
        
        return 


            
            


    
    examplenum()


;
      }(),
      sweet: () {
        PartReflection? _partReflection = _reflection.sweetPart;
        
        return UndefinedWrapper(


            
            


    
    examplebool()


);
      }(),
      
      
    );
  }
}

class BananaReqXmlReflection {
    const BananaReqXmlReflection();
}


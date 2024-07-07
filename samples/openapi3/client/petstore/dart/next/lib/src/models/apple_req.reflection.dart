// Model reflection

part of 'apple_req.dart';


//class reflection

class AppleReqReflection extends ClassReflection<AppleReq> {
  static AppleReqReflection instanceGetter() => instance;
  static const instance = AppleReqReflection._(
    modelName: r'appleReq',
    className: r'AppleReq',
    cultivarPart: PropertyReflection<AppleReq, 
            String
>(
      dartName: r'cultivar',
      nullable: false,
      required: true,
      oasName: r'cultivar',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _cultivarGetter,
      setter: _cultivarSetter,
    ),
    mealyPart: PropertyReflection<AppleReq, UndefinedWrapper<
            bool
>>(
      dartName: r'mealy',
      nullable: false,
      required: false,
      oasName: r'mealy',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _mealyGetter,
      setter: _mealySetter,
    ),
    
    
  );
  const AppleReqReflection._({
    required this.modelName,
    required this.className,
    required this.cultivarPart,
    required this.mealyPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
  });

  final PropertyReflection<AppleReq, 
            String
> cultivarPart;
  static 
            String
 _cultivarGetter(AppleReq parent) {
    return parent.cultivar;
  }
  static void _cultivarSetter(AppleReq parent, 
            String
 value) {
    parent.cultivar = value;
  }
  final PropertyReflection<AppleReq, UndefinedWrapper<
            bool
>> mealyPart;
  static UndefinedWrapper<
            bool
> _mealyGetter(AppleReq parent) {
    return parent.mealy;
  }
  static void _mealySetter(AppleReq parent, UndefinedWrapper<
            bool
> value) {
    parent.mealy = value;
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
  List<PropertyReflection<AppleReq, dynamic>> get properties => [
    cultivarPart,
mealyPart,
  ];

  
  
  
  @override
  List<PartReflection<AppleReq, dynamic>> get parts => [
    ...super.parts,
      ];
  @override
  List<AllOfReflection<AppleReq, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AppleReq.canDeserialize(src);
  @override
  AppleReq Function(Object? src) get deserializeFunction =>
      (src) => AppleReq.deserialize(src);

  @override
  Object? Function(AppleReq src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of AppleReq.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  AppleReq example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return AppleReq(
      cultivar: () {
        PartReflection? _partReflection = _reflection.cultivarPart;
        
        final disc = discriminators[r'cultivar'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return result;
          }
        }
        
        return 


            
            


    
    exampleString()


;
      }(),
      mealy: () {
        PartReflection? _partReflection = _reflection.mealyPart;
        
        return UndefinedWrapper(


            
            


    
    examplebool()


);
      }(),
      
      
    );
  }
}

class AppleReqXmlReflection {
    const AppleReqXmlReflection();
}


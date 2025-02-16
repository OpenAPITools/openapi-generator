// Model reflection

part of 'banana_req.dart';


//class reflection

class BananaReqReflection extends ModelReflection<BananaReq> {
  static BananaReqReflection instanceGetter() => instance;
  static const instance = BananaReqReflection._(
    modelName: r'bananaReq',
    className: r'BananaReq',
    xml: XmlReflection(
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_lengthCmGetter),
      setter: FunctionWrapper2(_lengthCmSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
,
)
,
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_sweetGetter),
      setter: FunctionWrapper2(_sweetSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
),
    ),
    
    
  );
  const BananaReqReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
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
  List<PropertyReflection<BananaReq, dynamic>> get properties => [
    lengthCmPart,
sweetPart,
  ];


  
  

  @override
  List<AllOfReflection<BananaReq, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  BananaReq empty() {
    return BananaReq(
      lengthCm: lengthCmPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is BananaReqReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



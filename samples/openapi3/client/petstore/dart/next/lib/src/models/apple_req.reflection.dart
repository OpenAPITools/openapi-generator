// Model reflection

part of 'apple_req.dart';


//class reflection

class AppleReqReflection extends ModelReflection<AppleReq> {
  static AppleReqReflection instanceGetter() => instance;
  static const instance = AppleReqReflection._(
    modelName: r'appleReq',
    className: r'AppleReq',
    xml: XmlReflection(
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_cultivarGetter),
      setter: FunctionWrapper2(_cultivarSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_mealyGetter),
      setter: FunctionWrapper2(_mealySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
),
    ),
    
    
  );
  const AppleReqReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
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
  List<PropertyReflection<AppleReq, dynamic>> get properties => [
    cultivarPart,
mealyPart,
  ];


  
  

  @override
  List<AllOfReflection<AppleReq, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  AppleReq empty() {
    return AppleReq(
      cultivar: cultivarPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is AppleReqReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



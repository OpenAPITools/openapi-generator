// Model reflection

part of 'fruit_req.dart';


//class reflection

class FruitReqReflection extends ModelReflection<FruitReq> {
  static FruitReqReflection instanceGetter() => instance;
  static const instance = FruitReqReflection._(
    modelName: r'fruitReq',
    className: r'FruitReq',
    xml: XmlReflection(
),
    
    
    oneOf0Part: FruitReqOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: FruitReqOneOf1Part(
      parentReflectionGetter: instanceGetter,
    ),
    
  );
  const FruitReqReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
  });


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
  List<PropertyReflection<FruitReq, dynamic>> get properties => [
      ];


  
  
  final FruitReqOneOf0Part oneOf0Part;
  
  final FruitReqOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<FruitReq, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<FruitReq, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<FruitReq, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  FruitReq empty() {
    return FruitReq(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is FruitReqReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class FruitReqOneOf0Part extends OneOfReflection<FruitReq, 
            AppleReq
> {

  const FruitReqOneOf0Part({
  required FruitReqReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            AppleReq
>, FruitReq> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, FruitReq, UndefinedWrapper<
            AppleReq
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            AppleReq
> _getter(FruitReq src) {
  return src.oneOf0;
}
static void _setter(FruitReq src, UndefinedWrapper<
            AppleReq
> value) {
  src.oneOf0 = value;
}

@override
UndefinedWrapperReflection<
            AppleReq
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                AppleReq.$reflection
        

  ),
);

  UndefinedWrapper<
            AppleReq
> example({
    required AggregatedDiscriminatorsResult discriminators,
    required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ModelReflection>> discriminatorExampleResults,
    required Map<String, Uint8List>? fileCache,
  }) {
    if (discriminatorExampleResults.isEmpty) {
      //return undefined for non-first oneOfs.
      // An example SHOULD be generated
    } else {
      // if this reflection wasn't a result of any property, don't generate an example.

      if (!discriminatorExampleResults.values
          .any((e) => e.value == reflection.subReflection)) {
        // if there are no discriminator examples targetting the current class:
        return UndefinedWrapper.undefined();
      } else {
        // An example SHOULD be generated
      }
    }
    return reflection.example();
  }
}

class FruitReqOneOf1Part extends OneOfReflection<FruitReq, 
            BananaReq
> {

  const FruitReqOneOf1Part({
  required FruitReqReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            BananaReq
>, FruitReq> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, FruitReq, UndefinedWrapper<
            BananaReq
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            BananaReq
> _getter(FruitReq src) {
  return src.oneOf1;
}
static void _setter(FruitReq src, UndefinedWrapper<
            BananaReq
> value) {
  src.oneOf1 = value;
}

@override
UndefinedWrapperReflection<
            BananaReq
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                BananaReq.$reflection
        

  ),
);

  UndefinedWrapper<
            BananaReq
> example({
    required AggregatedDiscriminatorsResult discriminators,
    required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ModelReflection>> discriminatorExampleResults,
    required Map<String, Uint8List>? fileCache,
  }) {
    if (discriminatorExampleResults.isEmpty) {
      //return undefined for non-first oneOfs.
      return UndefinedWrapper.undefined();
    } else {
      // if this reflection wasn't a result of any property, don't generate an example.

      if (!discriminatorExampleResults.values
          .any((e) => e.value == reflection.subReflection)) {
        // if there are no discriminator examples targetting the current class:
        return UndefinedWrapper.undefined();
      } else {
        // An example SHOULD be generated
      }
    }
    return reflection.example();
  }
}


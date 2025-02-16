// Model reflection

part of 'fake_one_of_w_ith_same_erasure_get200_response.dart';


//class reflection

class FakeOneOfWIthSameErasureGet200ResponseReflection extends ModelReflection<FakeOneOfWIthSameErasureGet200Response> {
  static FakeOneOfWIthSameErasureGet200ResponseReflection instanceGetter() => instance;
  static const instance = FakeOneOfWIthSameErasureGet200ResponseReflection._(
    modelName: r'_fake_oneOfWIthSameErasure_get_200_response',
    className: r'FakeOneOfWIthSameErasureGet200Response',
    xml: XmlReflection(
),
    
    
    oneOf0Part: FakeOneOfWIthSameErasureGet200ResponseOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: FakeOneOfWIthSameErasureGet200ResponseOneOf1Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(ObjectReflection()
),
)
,
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const FakeOneOfWIthSameErasureGet200ResponseReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.additionalPropertiesPart,
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
  List<PropertyReflection<FakeOneOfWIthSameErasureGet200Response, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<FakeOneOfWIthSameErasureGet200Response, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(FakeOneOfWIthSameErasureGet200Response instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(FakeOneOfWIthSameErasureGet200Response instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final FakeOneOfWIthSameErasureGet200ResponseOneOf0Part oneOf0Part;
  
  final FakeOneOfWIthSameErasureGet200ResponseOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<FakeOneOfWIthSameErasureGet200Response, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<FakeOneOfWIthSameErasureGet200Response, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<FakeOneOfWIthSameErasureGet200Response, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  FakeOneOfWIthSameErasureGet200Response empty() {
    return FakeOneOfWIthSameErasureGet200Response(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is FakeOneOfWIthSameErasureGet200ResponseReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class FakeOneOfWIthSameErasureGet200ResponseOneOf0Part extends OneOfReflection<FakeOneOfWIthSameErasureGet200Response, 
    List<
        
            String
>
> {

  const FakeOneOfWIthSameErasureGet200ResponseOneOf0Part({
  required FakeOneOfWIthSameErasureGet200ResponseReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
    List<
        
            String
>
>, FakeOneOfWIthSameErasureGet200Response> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, FakeOneOfWIthSameErasureGet200Response, UndefinedWrapper<
    List<
        
            String
>
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
    List<
        
            String
>
> _getter(FakeOneOfWIthSameErasureGet200Response src) {
  return src.oneOf0;
}
static void _setter(FakeOneOfWIthSameErasureGet200Response src, UndefinedWrapper<
    List<
        
            String
>
> value) {
  src.oneOf0 = value;
}

@override
UndefinedWrapperReflection<
    List<
        
            String
>
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
)

  ),
);

  UndefinedWrapper<
    List<
        
            String
>
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

class FakeOneOfWIthSameErasureGet200ResponseOneOf1Part extends OneOfReflection<FakeOneOfWIthSameErasureGet200Response, 
    List<
        
            int
>
> {

  const FakeOneOfWIthSameErasureGet200ResponseOneOf1Part({
  required FakeOneOfWIthSameErasureGet200ResponseReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
    List<
        
            int
>
>, FakeOneOfWIthSameErasureGet200Response> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, FakeOneOfWIthSameErasureGet200Response, UndefinedWrapper<
    List<
        
            int
>
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
    List<
        
            int
>
> _getter(FakeOneOfWIthSameErasureGet200Response src) {
  return src.oneOf1;
}
static void _setter(FakeOneOfWIthSameErasureGet200Response src, UndefinedWrapper<
    List<
        
            int
>
> value) {
  src.oneOf1 = value;
}

@override
UndefinedWrapperReflection<
    List<
        
            int
>
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
)

  ),
);

  UndefinedWrapper<
    List<
        
            int
>
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


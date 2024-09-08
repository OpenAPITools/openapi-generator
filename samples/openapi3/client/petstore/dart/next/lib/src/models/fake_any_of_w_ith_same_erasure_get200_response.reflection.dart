// Model reflection

part of 'fake_any_of_w_ith_same_erasure_get200_response.dart';


//class reflection

class FakeAnyOfWIthSameErasureGet200ResponseReflection extends ModelReflection<FakeAnyOfWIthSameErasureGet200Response> {
  static FakeAnyOfWIthSameErasureGet200ResponseReflection instanceGetter() => instance;
  static const instance = FakeAnyOfWIthSameErasureGet200ResponseReflection._(
    modelName: r'_fake_anyOfWIthSameErasure_get_200_response',
    className: r'FakeAnyOfWIthSameErasureGet200Response',
    xml: XmlReflection(
),
    
    
    anyOf0Part: FakeAnyOfWIthSameErasureGet200ResponseAnyOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    anyOf1Part: FakeAnyOfWIthSameErasureGet200ResponseAnyOf1Part(
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
  const FakeAnyOfWIthSameErasureGet200ResponseReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.anyOf0Part,
    
    required this.anyOf1Part,
    
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
  List<PropertyReflection<FakeAnyOfWIthSameErasureGet200Response, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<FakeAnyOfWIthSameErasureGet200Response, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(FakeAnyOfWIthSameErasureGet200Response instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(FakeAnyOfWIthSameErasureGet200Response instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final FakeAnyOfWIthSameErasureGet200ResponseAnyOf0Part anyOf0Part;
  
  final FakeAnyOfWIthSameErasureGet200ResponseAnyOf1Part anyOf1Part;
  

  @override
  List<AllOfReflection<FakeAnyOfWIthSameErasureGet200Response, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<FakeAnyOfWIthSameErasureGet200Response, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<FakeAnyOfWIthSameErasureGet200Response, Object>> get anyOfs => [
    anyOf0Part,anyOf1Part,
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  FakeAnyOfWIthSameErasureGet200Response empty() {
    return FakeAnyOfWIthSameErasureGet200Response(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is FakeAnyOfWIthSameErasureGet200ResponseReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}

class FakeAnyOfWIthSameErasureGet200ResponseAnyOf0Part extends AnyOfReflection<FakeAnyOfWIthSameErasureGet200Response, 
    List<
        
            String
>
> {

  const FakeAnyOfWIthSameErasureGet200ResponseAnyOf0Part({
  required FakeAnyOfWIthSameErasureGet200ResponseReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
    List<
        
            String
>
>, FakeAnyOfWIthSameErasureGet200Response> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, FakeAnyOfWIthSameErasureGet200Response, UndefinedWrapper<
    List<
        
            String
>
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
    List<
        
            String
>
> _getter(FakeAnyOfWIthSameErasureGet200Response src) {
  return src.anyOf0;
}
static void _setter(FakeAnyOfWIthSameErasureGet200Response src, UndefinedWrapper<
    List<
        
            String
>
> value) {
  src.anyOf0 = value;
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
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == reflection.subReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return reflection.example();
  }
}
class FakeAnyOfWIthSameErasureGet200ResponseAnyOf1Part extends AnyOfReflection<FakeAnyOfWIthSameErasureGet200Response, 
    List<
        
            int
>
> {

  const FakeAnyOfWIthSameErasureGet200ResponseAnyOf1Part({
  required FakeAnyOfWIthSameErasureGet200ResponseReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
    List<
        
            int
>
>, FakeAnyOfWIthSameErasureGet200Response> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, FakeAnyOfWIthSameErasureGet200Response, UndefinedWrapper<
    List<
        
            int
>
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
    List<
        
            int
>
> _getter(FakeAnyOfWIthSameErasureGet200Response src) {
  return src.anyOf1;
}
static void _setter(FakeAnyOfWIthSameErasureGet200Response src, UndefinedWrapper<
    List<
        
            int
>
> value) {
  src.anyOf1 = value;
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
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == reflection.subReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return reflection.example();
  }
}


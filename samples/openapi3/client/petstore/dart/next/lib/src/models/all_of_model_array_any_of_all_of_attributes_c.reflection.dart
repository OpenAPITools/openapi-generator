// Model reflection

part of 'all_of_model_array_any_of_all_of_attributes_c.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfAttributesCReflection extends ModelReflection<AllOfModelArrayAnyOfAllOfAttributesC> {
  static AllOfModelArrayAnyOfAllOfAttributesCReflection instanceGetter() => instance;
  static const instance = AllOfModelArrayAnyOfAllOfAttributesCReflection._(
    modelName: r'AllOfModelArrayAnyOf_allOf_attributes_C',
    className: r'AllOfModelArrayAnyOfAllOfAttributesC',
    xml: XmlReflection(
),
    
    
    oneOf0Part: AllOfModelArrayAnyOfAllOfAttributesCOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: AllOfModelArrayAnyOfAllOfAttributesCOneOf1Part(
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
  const AllOfModelArrayAnyOfAllOfAttributesCReflection._({
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
  List<PropertyReflection<AllOfModelArrayAnyOfAllOfAttributesC, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<AllOfModelArrayAnyOfAllOfAttributesC, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(AllOfModelArrayAnyOfAllOfAttributesC instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(AllOfModelArrayAnyOfAllOfAttributesC instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final AllOfModelArrayAnyOfAllOfAttributesCOneOf0Part oneOf0Part;
  
  final AllOfModelArrayAnyOfAllOfAttributesCOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  AllOfModelArrayAnyOfAllOfAttributesC empty() {
    return AllOfModelArrayAnyOfAllOfAttributesC(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is AllOfModelArrayAnyOfAllOfAttributesCReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class AllOfModelArrayAnyOfAllOfAttributesCOneOf0Part extends OneOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, 
            Pet
> {

  const AllOfModelArrayAnyOfAllOfAttributesCOneOf0Part({
  required AllOfModelArrayAnyOfAllOfAttributesCReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Pet
>, AllOfModelArrayAnyOfAllOfAttributesC> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, AllOfModelArrayAnyOfAllOfAttributesC, UndefinedWrapper<
            Pet
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Pet
> _getter(AllOfModelArrayAnyOfAllOfAttributesC src) {
  return src.oneOf0;
}
static void _setter(AllOfModelArrayAnyOfAllOfAttributesC src, UndefinedWrapper<
            Pet
> value) {
  src.oneOf0 = value;
}

@override
UndefinedWrapperReflection<
            Pet
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Pet',
),
    
            
        
        
            
                Pet.$reflection
        

  ),
);

  UndefinedWrapper<
            Pet
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

class AllOfModelArrayAnyOfAllOfAttributesCOneOf1Part extends OneOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, 
            Order
> {

  const AllOfModelArrayAnyOfAllOfAttributesCOneOf1Part({
  required AllOfModelArrayAnyOfAllOfAttributesCReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Order
>, AllOfModelArrayAnyOfAllOfAttributesC> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, AllOfModelArrayAnyOfAllOfAttributesC, UndefinedWrapper<
            Order
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Order
> _getter(AllOfModelArrayAnyOfAllOfAttributesC src) {
  return src.oneOf1;
}
static void _setter(AllOfModelArrayAnyOfAllOfAttributesC src, UndefinedWrapper<
            Order
> value) {
  src.oneOf1 = value;
}

@override
UndefinedWrapperReflection<
            Order
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Order',
),
    
            
        
        
            
                Order.$reflection
        

  ),
);

  UndefinedWrapper<
            Order
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


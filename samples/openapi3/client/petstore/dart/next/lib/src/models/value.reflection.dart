// Model reflection

part of 'value.dart';


//class reflection

class ValueReflection extends ModelReflection<Value> {
  static ValueReflection instanceGetter() => instance;
  static const instance = ValueReflection._(
    modelName: r'Value',
    className: r'Value',
    xml: XmlReflection(
),
    
    
    oneOf0Part: ValueOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: ValueOneOf1Part(
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
  const ValueReflection._({
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
  List<PropertyReflection<Value, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<Value, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Value instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Value instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final ValueOneOf0Part oneOf0Part;
  
  final ValueOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<Value, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Value, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<Value, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Value empty() {
    return Value(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ValueReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class ValueOneOf0Part extends OneOfReflection<Value, 
            Scalar
> {

  const ValueOneOf0Part({
  required ValueReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Scalar
>, Value> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Value, UndefinedWrapper<
            Scalar
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Scalar
> _getter(Value src) {
  return src.oneOf0;
}
static void _setter(Value src, UndefinedWrapper<
            Scalar
> value) {
  src.oneOf0 = value;
}

@override
UndefinedWrapperReflection<
            Scalar
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Scalar.$reflection
        

  ),
);

  UndefinedWrapper<
            Scalar
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

class ValueOneOf1Part extends OneOfReflection<Value, 
    List<
        
            Scalar
>
> {

  const ValueOneOf1Part({
  required ValueReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
    List<
        
            Scalar
>
>, Value> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Value, UndefinedWrapper<
    List<
        
            Scalar
>
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
    List<
        
            Scalar
>
> _getter(Value src) {
  return src.oneOf1;
}
static void _setter(Value src, UndefinedWrapper<
    List<
        
            Scalar
>
> value) {
  src.oneOf1 = value;
}

@override
UndefinedWrapperReflection<
    List<
        
            Scalar
>
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Scalar.$reflection
        
,
)
)

  ),
);

  UndefinedWrapper<
    List<
        
            Scalar
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


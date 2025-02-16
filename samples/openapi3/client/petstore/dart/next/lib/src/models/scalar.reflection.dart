// Model reflection

part of 'scalar.dart';


//class reflection

class ScalarReflection extends ModelReflection<Scalar> {
  static ScalarReflection instanceGetter() => instance;
  static const instance = ScalarReflection._(
    modelName: r'Scalar',
    className: r'Scalar',
    xml: XmlReflection(
),
    
    
    oneOf0Part: ScalarOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: ScalarOneOf1Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf2Part: ScalarOneOf2Part(
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
  const ScalarReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.oneOf2Part,
    
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
  List<PropertyReflection<Scalar, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<Scalar, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Scalar instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Scalar instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final ScalarOneOf0Part oneOf0Part;
  
  final ScalarOneOf1Part oneOf1Part;
  
  final ScalarOneOf2Part oneOf2Part;
  

  @override
  List<AllOfReflection<Scalar, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Scalar, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,oneOf2Part,
  ];
  @override
  List<AnyOfReflection<Scalar, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Scalar empty() {
    return Scalar(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ScalarReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class ScalarOneOf0Part extends OneOfReflection<Scalar, 
            String
> {

  const ScalarOneOf0Part({
  required ScalarReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            String
>, Scalar> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Scalar, UndefinedWrapper<
            String
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            String
> _getter(Scalar src) {
  return src.oneOf0;
}
static void _setter(Scalar src, UndefinedWrapper<
            String
> value) {
  src.oneOf0 = value;
}

@override
UndefinedWrapperReflection<
            String
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        

  ),
);

  UndefinedWrapper<
            String
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

class ScalarOneOf1Part extends OneOfReflection<Scalar, 
            num
> {

  const ScalarOneOf1Part({
  required ScalarReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            num
>, Scalar> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Scalar, UndefinedWrapper<
            num
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            num
> _getter(Scalar src) {
  return src.oneOf1;
}
static void _setter(Scalar src, UndefinedWrapper<
            num
> value) {
  src.oneOf1 = value;
}

@override
UndefinedWrapperReflection<
            num
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fornum
        

  ),
);

  UndefinedWrapper<
            num
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

class ScalarOneOf2Part extends OneOfReflection<Scalar, 
            bool
> {

  const ScalarOneOf2Part({
  required ScalarReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            bool
>, Scalar> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Scalar, UndefinedWrapper<
            bool
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            bool
> _getter(Scalar src) {
  return src.oneOf2;
}
static void _setter(Scalar src, UndefinedWrapper<
            bool
> value) {
  src.oneOf2 = value;
}

@override
UndefinedWrapperReflection<
            bool
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        

  ),
);

  UndefinedWrapper<
            bool
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


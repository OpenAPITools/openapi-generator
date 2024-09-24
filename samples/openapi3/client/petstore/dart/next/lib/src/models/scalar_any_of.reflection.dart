// Model reflection

part of 'scalar_any_of.dart';


//class reflection

class ScalarAnyOfReflection extends ModelReflection<ScalarAnyOf> {
  static ScalarAnyOfReflection instanceGetter() => instance;
  static const instance = ScalarAnyOfReflection._(
    modelName: r'ScalarAnyOf',
    className: r'ScalarAnyOf',
    xml: XmlReflection(
),
    
    
    anyOf0Part: ScalarAnyOfAnyOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    anyOf1Part: ScalarAnyOfAnyOf1Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    anyOf2Part: ScalarAnyOfAnyOf2Part(
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
  const ScalarAnyOfReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.anyOf0Part,
    
    required this.anyOf1Part,
    
    required this.anyOf2Part,
    
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
  List<PropertyReflection<ScalarAnyOf, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<ScalarAnyOf, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ScalarAnyOf instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ScalarAnyOf instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final ScalarAnyOfAnyOf0Part anyOf0Part;
  
  final ScalarAnyOfAnyOf1Part anyOf1Part;
  
  final ScalarAnyOfAnyOf2Part anyOf2Part;
  

  @override
  List<AllOfReflection<ScalarAnyOf, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ScalarAnyOf, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ScalarAnyOf, Object>> get anyOfs => [
    anyOf0Part,anyOf1Part,anyOf2Part,
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ScalarAnyOf empty() {
    return ScalarAnyOf(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ScalarAnyOfReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}

class ScalarAnyOfAnyOf0Part extends AnyOfReflection<ScalarAnyOf, 
            String
> {

  const ScalarAnyOfAnyOf0Part({
  required ScalarAnyOfReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            String
>, ScalarAnyOf> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, ScalarAnyOf, UndefinedWrapper<
            String
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            String
> _getter(ScalarAnyOf src) {
  return src.anyOf0;
}
static void _setter(ScalarAnyOf src, UndefinedWrapper<
            String
> value) {
  src.anyOf0 = value;
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
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == reflection.subReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return reflection.example();
  }
}
class ScalarAnyOfAnyOf1Part extends AnyOfReflection<ScalarAnyOf, 
            num
> {

  const ScalarAnyOfAnyOf1Part({
  required ScalarAnyOfReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            num
>, ScalarAnyOf> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, ScalarAnyOf, UndefinedWrapper<
            num
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            num
> _getter(ScalarAnyOf src) {
  return src.anyOf1;
}
static void _setter(ScalarAnyOf src, UndefinedWrapper<
            num
> value) {
  src.anyOf1 = value;
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
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == reflection.subReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return reflection.example();
  }
}
class ScalarAnyOfAnyOf2Part extends AnyOfReflection<ScalarAnyOf, 
            bool
> {

  const ScalarAnyOfAnyOf2Part({
  required ScalarAnyOfReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            bool
>, ScalarAnyOf> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, ScalarAnyOf, UndefinedWrapper<
            bool
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            bool
> _getter(ScalarAnyOf src) {
  return src.anyOf2;
}
static void _setter(ScalarAnyOf src, UndefinedWrapper<
            bool
> value) {
  src.anyOf2 = value;
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
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == reflection.subReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return reflection.example();
  }
}


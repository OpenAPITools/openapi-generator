// Model reflection

part of 'free_form_object_test_class_properties.dart';


//class reflection

class FreeFormObjectTestClassPropertiesReflection extends ModelReflection<FreeFormObjectTestClassProperties> {
  static FreeFormObjectTestClassPropertiesReflection instanceGetter() => instance;
  static const instance = FreeFormObjectTestClassPropertiesReflection._(
    modelName: r'FreeFormObjectTestClass_properties',
    className: r'FreeFormObjectTestClassProperties',
    xml: XmlReflection(
),
    
    
    oneOf0Part: FreeFormObjectTestClassPropertiesOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: FreeFormObjectTestClassPropertiesOneOf1Part(
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
  const FreeFormObjectTestClassPropertiesReflection._({
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
  List<PropertyReflection<FreeFormObjectTestClassProperties, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<FreeFormObjectTestClassProperties, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(FreeFormObjectTestClassProperties instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(FreeFormObjectTestClassProperties instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final FreeFormObjectTestClassPropertiesOneOf0Part oneOf0Part;
  
  final FreeFormObjectTestClassPropertiesOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<FreeFormObjectTestClassProperties, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<FreeFormObjectTestClassProperties, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<FreeFormObjectTestClassProperties, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  FreeFormObjectTestClassProperties empty() {
    return FreeFormObjectTestClassProperties(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is FreeFormObjectTestClassPropertiesReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class FreeFormObjectTestClassPropertiesOneOf0Part extends OneOfReflection<FreeFormObjectTestClassProperties, 
            String
> {

  const FreeFormObjectTestClassPropertiesOneOf0Part({
  required FreeFormObjectTestClassPropertiesReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            String
>, FreeFormObjectTestClassProperties> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, FreeFormObjectTestClassProperties, UndefinedWrapper<
            String
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            String
> _getter(FreeFormObjectTestClassProperties src) {
  return src.oneOf0;
}
static void _setter(FreeFormObjectTestClassProperties src, UndefinedWrapper<
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

class FreeFormObjectTestClassPropertiesOneOf1Part extends OneOfReflection<FreeFormObjectTestClassProperties, 
    Map<String, 
        Object
?>
> {

  const FreeFormObjectTestClassPropertiesOneOf1Part({
  required FreeFormObjectTestClassPropertiesReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
    Map<String, 
        Object
?>
>, FreeFormObjectTestClassProperties> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, FreeFormObjectTestClassProperties, UndefinedWrapper<
    Map<String, 
        Object
?>
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
    Map<String, 
        Object
?>
> _getter(FreeFormObjectTestClassProperties src) {
  return src.oneOf1;
}
static void _setter(FreeFormObjectTestClassProperties src, UndefinedWrapper<
    Map<String, 
        Object
?>
> value) {
  src.oneOf1 = value;
}

@override
UndefinedWrapperReflection<
    Map<String, 
        Object
?>
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    MapReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(ObjectReflection()
),
)
)

  ),
);

  UndefinedWrapper<
    Map<String, 
        Object
?>
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


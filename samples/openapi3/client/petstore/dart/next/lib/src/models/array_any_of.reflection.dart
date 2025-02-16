// Model reflection

part of 'array_any_of.dart';


//class reflection

class ArrayAnyOfReflection extends ModelReflection<ArrayAnyOf> {
  static ArrayAnyOfReflection instanceGetter() => instance;
  static const instance = ArrayAnyOfReflection._(
    modelName: r'ArrayAnyOf',
    className: r'ArrayAnyOf',
    xml: XmlReflection(
),
    
    
    anyOf0Part: ArrayAnyOfAnyOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    anyOf1Part: ArrayAnyOfAnyOf1Part(
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
  const ArrayAnyOfReflection._({
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
  List<PropertyReflection<ArrayAnyOf, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<ArrayAnyOf, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ArrayAnyOf instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ArrayAnyOf instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final ArrayAnyOfAnyOf0Part anyOf0Part;
  
  final ArrayAnyOfAnyOf1Part anyOf1Part;
  

  @override
  List<AllOfReflection<ArrayAnyOf, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ArrayAnyOf, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ArrayAnyOf, Object>> get anyOfs => [
    anyOf0Part,anyOf1Part,
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ArrayAnyOf empty() {
    return ArrayAnyOf(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ArrayAnyOfReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}

class ArrayAnyOfAnyOf0Part extends AnyOfReflection<ArrayAnyOf, 
            int
> {

  const ArrayAnyOfAnyOf0Part({
  required ArrayAnyOfReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            int
>, ArrayAnyOf> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, ArrayAnyOf, UndefinedWrapper<
            int
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            int
> _getter(ArrayAnyOf src) {
  return src.anyOf0;
}
static void _setter(ArrayAnyOf src, UndefinedWrapper<
            int
> value) {
  src.anyOf0 = value;
}

@override
UndefinedWrapperReflection<
            int
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        

  ),
);

  UndefinedWrapper<
            int
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
class ArrayAnyOfAnyOf1Part extends AnyOfReflection<ArrayAnyOf, 
    List<
        
            String
>
> {

  const ArrayAnyOfAnyOf1Part({
  required ArrayAnyOfReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
    List<
        
            String
>
>, ArrayAnyOf> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, ArrayAnyOf, UndefinedWrapper<
    List<
        
            String
>
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
    List<
        
            String
>
> _getter(ArrayAnyOf src) {
  return src.anyOf1;
}
static void _setter(ArrayAnyOf src, UndefinedWrapper<
    List<
        
            String
>
> value) {
  src.anyOf1 = value;
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


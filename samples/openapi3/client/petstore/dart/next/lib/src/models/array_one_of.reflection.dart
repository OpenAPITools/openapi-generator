// Model reflection

part of 'array_one_of.dart';


//class reflection

class ArrayOneOfReflection extends ModelReflection<ArrayOneOf> {
  static ArrayOneOfReflection instanceGetter() => instance;
  static const instance = ArrayOneOfReflection._(
    modelName: r'ArrayOneOf',
    className: r'ArrayOneOf',
    xml: XmlReflection(
),
    
    
    oneOf0Part: ArrayOneOfOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: ArrayOneOfOneOf1Part(
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
  const ArrayOneOfReflection._({
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
  List<PropertyReflection<ArrayOneOf, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<ArrayOneOf, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ArrayOneOf instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ArrayOneOf instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final ArrayOneOfOneOf0Part oneOf0Part;
  
  final ArrayOneOfOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<ArrayOneOf, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ArrayOneOf, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<ArrayOneOf, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ArrayOneOf empty() {
    return ArrayOneOf(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ArrayOneOfReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class ArrayOneOfOneOf0Part extends OneOfReflection<ArrayOneOf, 
            int
> {

  const ArrayOneOfOneOf0Part({
  required ArrayOneOfReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            int
>, ArrayOneOf> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, ArrayOneOf, UndefinedWrapper<
            int
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            int
> _getter(ArrayOneOf src) {
  return src.oneOf0;
}
static void _setter(ArrayOneOf src, UndefinedWrapper<
            int
> value) {
  src.oneOf0 = value;
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

class ArrayOneOfOneOf1Part extends OneOfReflection<ArrayOneOf, 
    List<
        
            String
>
> {

  const ArrayOneOfOneOf1Part({
  required ArrayOneOfReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
    List<
        
            String
>
>, ArrayOneOf> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, ArrayOneOf, UndefinedWrapper<
    List<
        
            String
>
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
    List<
        
            String
>
> _getter(ArrayOneOf src) {
  return src.oneOf1;
}
static void _setter(ArrayOneOf src, UndefinedWrapper<
    List<
        
            String
>
> value) {
  src.oneOf1 = value;
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


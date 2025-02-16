// Model reflection

part of 'triangle.dart';


//class reflection

class TriangleReflection extends ModelReflection<Triangle> {
  static TriangleReflection instanceGetter() => instance;
  static const instance = TriangleReflection._(
    modelName: r'Triangle',
    className: r'Triangle',
    xml: XmlReflection(
),
    triangleTypePart: PropertyReflection<Triangle, 
            String
>(
      dartName: r'triangleType',
      nullable: false,
      required: true,
      oasName: r'triangleType',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: true,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_triangleTypeGetter),
      setter: FunctionWrapper2(_triangleTypeSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
    ),
    discriminatorKey: r'triangleType',
    discriminatorImplicitMappings: const {
      r'EquilateralTriangle': EquilateralTriangleReflection.instance,
      r'IsoscelesTriangle': IsoscelesTriangleReflection.instance,
      r'ScaleneTriangle': ScaleneTriangleReflection.instance,
    },
    discriminatorMappings: const {
      r'EquilateralTriangle': EquilateralTriangleReflection.instance,
      r'IsoscelesTriangle': IsoscelesTriangleReflection.instance,
      r'ScaleneTriangle': ScaleneTriangleReflection.instance,
    },
    
    
    oneOf0Part: TriangleOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: TriangleOneOf1Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf2Part: TriangleOneOf2Part(
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
  const TriangleReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.triangleTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.oneOf2Part,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Triangle, 
            String
> triangleTypePart;
  static 
            String
 _triangleTypeGetter(Triangle parent) {
    return parent.triangleType;
  }
  static void _triangleTypeSetter(Triangle parent, 
            String
 value) {
    parent.triangleType = value;
  }


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
  List<PropertyReflection<Triangle, dynamic>> get properties => [
    triangleTypePart,
  ];

  @override
  final AdditionalPropertiesPart<Triangle, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Triangle instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Triangle instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final TriangleOneOf0Part oneOf0Part;
  
  final TriangleOneOf1Part oneOf1Part;
  
  final TriangleOneOf2Part oneOf2Part;
  

  @override
  List<AllOfReflection<Triangle, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Triangle, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,oneOf2Part,
  ];
  @override
  List<AnyOfReflection<Triangle, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Triangle empty() {
    return Triangle(
      triangleType: triangleTypePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is TriangleReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class TriangleOneOf0Part extends OneOfReflection<Triangle, 
            EquilateralTriangle
> {

  const TriangleOneOf0Part({
  required TriangleReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            EquilateralTriangle
>, Triangle> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Triangle, UndefinedWrapper<
            EquilateralTriangle
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            EquilateralTriangle
> _getter(Triangle src) {
  return src.oneOf0;
}
static void _setter(Triangle src, UndefinedWrapper<
            EquilateralTriangle
> value) {
  src.oneOf0 = value;
}

@override
UndefinedWrapperReflection<
            EquilateralTriangle
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                EquilateralTriangle.$reflection
        

  ),
);

  UndefinedWrapper<
            EquilateralTriangle
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

class TriangleOneOf1Part extends OneOfReflection<Triangle, 
            IsoscelesTriangle
> {

  const TriangleOneOf1Part({
  required TriangleReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            IsoscelesTriangle
>, Triangle> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Triangle, UndefinedWrapper<
            IsoscelesTriangle
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            IsoscelesTriangle
> _getter(Triangle src) {
  return src.oneOf1;
}
static void _setter(Triangle src, UndefinedWrapper<
            IsoscelesTriangle
> value) {
  src.oneOf1 = value;
}

@override
UndefinedWrapperReflection<
            IsoscelesTriangle
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                IsoscelesTriangle.$reflection
        

  ),
);

  UndefinedWrapper<
            IsoscelesTriangle
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

class TriangleOneOf2Part extends OneOfReflection<Triangle, 
            ScaleneTriangle
> {

  const TriangleOneOf2Part({
  required TriangleReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            ScaleneTriangle
>, Triangle> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Triangle, UndefinedWrapper<
            ScaleneTriangle
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            ScaleneTriangle
> _getter(Triangle src) {
  return src.oneOf2;
}
static void _setter(Triangle src, UndefinedWrapper<
            ScaleneTriangle
> value) {
  src.oneOf2 = value;
}

@override
UndefinedWrapperReflection<
            ScaleneTriangle
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                ScaleneTriangle.$reflection
        

  ),
);

  UndefinedWrapper<
            ScaleneTriangle
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


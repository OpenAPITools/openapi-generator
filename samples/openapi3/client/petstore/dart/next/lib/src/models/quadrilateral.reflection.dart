// Model reflection

part of 'quadrilateral.dart';


//class reflection

class QuadrilateralReflection extends ModelReflection<Quadrilateral> {
  static QuadrilateralReflection instanceGetter() => instance;
  static const instance = QuadrilateralReflection._(
    modelName: r'Quadrilateral',
    className: r'Quadrilateral',
    xml: XmlReflection(
),
    quadrilateralTypePart: PropertyReflection<Quadrilateral, 
            String
>(
      dartName: r'quadrilateralType',
      nullable: false,
      required: true,
      oasName: r'quadrilateralType',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: true,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_quadrilateralTypeGetter),
      setter: FunctionWrapper2(_quadrilateralTypeSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
    ),
    discriminatorKey: r'quadrilateralType',
    discriminatorImplicitMappings: const {
      r'ComplexQuadrilateral': ComplexQuadrilateralReflection.instance,
      r'SimpleQuadrilateral': SimpleQuadrilateralReflection.instance,
    },
    discriminatorMappings: const {
      r'ComplexQuadrilateral': ComplexQuadrilateralReflection.instance,
      r'SimpleQuadrilateral': SimpleQuadrilateralReflection.instance,
    },
    
    
    oneOf0Part: QuadrilateralOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: QuadrilateralOneOf1Part(
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
  const QuadrilateralReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.quadrilateralTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Quadrilateral, 
            String
> quadrilateralTypePart;
  static 
            String
 _quadrilateralTypeGetter(Quadrilateral parent) {
    return parent.quadrilateralType;
  }
  static void _quadrilateralTypeSetter(Quadrilateral parent, 
            String
 value) {
    parent.quadrilateralType = value;
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
  List<PropertyReflection<Quadrilateral, dynamic>> get properties => [
    quadrilateralTypePart,
  ];

  @override
  final AdditionalPropertiesPart<Quadrilateral, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Quadrilateral instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Quadrilateral instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final QuadrilateralOneOf0Part oneOf0Part;
  
  final QuadrilateralOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<Quadrilateral, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Quadrilateral, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<Quadrilateral, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Quadrilateral empty() {
    return Quadrilateral(
      quadrilateralType: quadrilateralTypePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is QuadrilateralReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class QuadrilateralOneOf0Part extends OneOfReflection<Quadrilateral, 
            SimpleQuadrilateral
> {

  const QuadrilateralOneOf0Part({
  required QuadrilateralReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            SimpleQuadrilateral
>, Quadrilateral> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Quadrilateral, UndefinedWrapper<
            SimpleQuadrilateral
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            SimpleQuadrilateral
> _getter(Quadrilateral src) {
  return src.oneOf0;
}
static void _setter(Quadrilateral src, UndefinedWrapper<
            SimpleQuadrilateral
> value) {
  src.oneOf0 = value;
}

@override
UndefinedWrapperReflection<
            SimpleQuadrilateral
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                SimpleQuadrilateral.$reflection
        

  ),
);

  UndefinedWrapper<
            SimpleQuadrilateral
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

class QuadrilateralOneOf1Part extends OneOfReflection<Quadrilateral, 
            ComplexQuadrilateral
> {

  const QuadrilateralOneOf1Part({
  required QuadrilateralReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            ComplexQuadrilateral
>, Quadrilateral> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Quadrilateral, UndefinedWrapper<
            ComplexQuadrilateral
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            ComplexQuadrilateral
> _getter(Quadrilateral src) {
  return src.oneOf1;
}
static void _setter(Quadrilateral src, UndefinedWrapper<
            ComplexQuadrilateral
> value) {
  src.oneOf1 = value;
}

@override
UndefinedWrapperReflection<
            ComplexQuadrilateral
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                ComplexQuadrilateral.$reflection
        

  ),
);

  UndefinedWrapper<
            ComplexQuadrilateral
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


// Model reflection

part of 'shape.dart';


//class reflection

class ShapeReflection extends ModelReflection<Shape> {
  static ShapeReflection instanceGetter() => instance;
  static const instance = ShapeReflection._(
    modelName: r'Shape',
    className: r'Shape',
    xml: XmlReflection(
),
    discriminatorKey: r'shapeType',
    discriminatorImplicitMappings: const {
      r'Quadrilateral': QuadrilateralReflection.instance,
      r'Triangle': TriangleReflection.instance,
    },
    discriminatorMappings: const {
      r'Quadrilateral': QuadrilateralReflection.instance,
      r'Triangle': TriangleReflection.instance,
    },
    
    
    oneOf0Part: ShapeOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: ShapeOneOf1Part(
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
  const ShapeReflection._({
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
  List<PropertyReflection<Shape, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<Shape, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Shape instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Shape instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final ShapeOneOf0Part oneOf0Part;
  
  final ShapeOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<Shape, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Shape, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<Shape, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Shape empty() {
    return Shape(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ShapeReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class ShapeOneOf0Part extends OneOfReflection<Shape, 
            Triangle
> {

  const ShapeOneOf0Part({
  required ShapeReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Triangle
>, Shape> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Shape, UndefinedWrapper<
            Triangle
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Triangle
> _getter(Shape src) {
  return src.oneOf0;
}
static void _setter(Shape src, UndefinedWrapper<
            Triangle
> value) {
  src.oneOf0 = value;
}

@override
UndefinedWrapperReflection<
            Triangle
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Triangle.$reflection
        

  ),
);

  UndefinedWrapper<
            Triangle
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

class ShapeOneOf1Part extends OneOfReflection<Shape, 
            Quadrilateral
> {

  const ShapeOneOf1Part({
  required ShapeReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Quadrilateral
>, Shape> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, Shape, UndefinedWrapper<
            Quadrilateral
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Quadrilateral
> _getter(Shape src) {
  return src.oneOf1;
}
static void _setter(Shape src, UndefinedWrapper<
            Quadrilateral
> value) {
  src.oneOf1 = value;
}

@override
UndefinedWrapperReflection<
            Quadrilateral
> get reflection => UndefinedWrapperReflection(
  XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Quadrilateral.$reflection
        

  ),
);

  UndefinedWrapper<
            Quadrilateral
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


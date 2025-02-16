// Model reflection

part of 'shape_or_null.dart';


//class reflection

class ShapeOrNullReflection extends ModelReflection<ShapeOrNull> {
  static ShapeOrNullReflection instanceGetter() => instance;
  static const instance = ShapeOrNullReflection._(
    modelName: r'ShapeOrNull',
    className: r'ShapeOrNull',
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
    
    
    oneOf0Part: ShapeOrNullOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: ShapeOrNullOneOf1Part(
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
  const ShapeOrNullReflection._({
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
  List<PropertyReflection<ShapeOrNull, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<ShapeOrNull, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ShapeOrNull instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ShapeOrNull instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final ShapeOrNullOneOf0Part oneOf0Part;
  
  final ShapeOrNullOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<ShapeOrNull, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ShapeOrNull, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<ShapeOrNull, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ShapeOrNull empty() {
    return ShapeOrNull(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ShapeOrNullReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class ShapeOrNullOneOf0Part extends OneOfReflection<ShapeOrNull, 
            Triangle
> {

  const ShapeOrNullOneOf0Part({
  required ShapeOrNullReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Triangle
>, ShapeOrNull> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, ShapeOrNull, UndefinedWrapper<
            Triangle
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Triangle
> _getter(ShapeOrNull src) {
  return src.oneOf0;
}
static void _setter(ShapeOrNull src, UndefinedWrapper<
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

class ShapeOrNullOneOf1Part extends OneOfReflection<ShapeOrNull, 
            Quadrilateral
> {

  const ShapeOrNullOneOf1Part({
  required ShapeOrNullReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Quadrilateral
>, ShapeOrNull> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, ShapeOrNull, UndefinedWrapper<
            Quadrilateral
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Quadrilateral
> _getter(ShapeOrNull src) {
  return src.oneOf1;
}
static void _setter(ShapeOrNull src, UndefinedWrapper<
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


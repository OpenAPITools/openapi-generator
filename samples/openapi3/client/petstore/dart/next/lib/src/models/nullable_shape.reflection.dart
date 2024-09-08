// Model reflection

part of 'nullable_shape.dart';


//class reflection

class NullableShapeReflection extends ModelReflection<NullableShape> {
  static NullableShapeReflection instanceGetter() => instance;
  static const instance = NullableShapeReflection._(
    modelName: r'NullableShape',
    className: r'NullableShape',
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
    
    
    oneOf0Part: NullableShapeOneOf0Part(
      parentReflectionGetter: instanceGetter,
    ),
    
    oneOf1Part: NullableShapeOneOf1Part(
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
  const NullableShapeReflection._({
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
  List<PropertyReflection<NullableShape, dynamic>> get properties => [
      ];

  @override
  final AdditionalPropertiesPart<NullableShape, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(NullableShape instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(NullableShape instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  
  final NullableShapeOneOf0Part oneOf0Part;
  
  final NullableShapeOneOf1Part oneOf1Part;
  

  @override
  List<AllOfReflection<NullableShape, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<NullableShape, Object>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<NullableShape, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  NullableShape empty() {
    return NullableShape(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is NullableShapeReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


class NullableShapeOneOf0Part extends OneOfReflection<NullableShape, 
            Triangle
> {

  const NullableShapeOneOf0Part({
  required NullableShapeReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Triangle
>, NullableShape> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, NullableShape, UndefinedWrapper<
            Triangle
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Triangle
> _getter(NullableShape src) {
  return src.oneOf0;
}
static void _setter(NullableShape src, UndefinedWrapper<
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

class NullableShapeOneOf1Part extends OneOfReflection<NullableShape, 
            Quadrilateral
> {

  const NullableShapeOneOf1Part({
  required NullableShapeReflection Function() super.parentReflectionGetter,
});
@override
FunctionWrapper1<UndefinedWrapper<
            Quadrilateral
>, NullableShape> get getter => FunctionWrapper1(_getter);
@override
FunctionWrapper2<void, NullableShape, UndefinedWrapper<
            Quadrilateral
>> get setter => FunctionWrapper2(_setter);

static UndefinedWrapper<
            Quadrilateral
> _getter(NullableShape src) {
  return src.oneOf1;
}
static void _setter(NullableShape src, UndefinedWrapper<
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


// Model reflection

part of 'drawing.dart';


//class reflection

class DrawingReflection extends ModelReflection<Drawing> {
  static DrawingReflection instanceGetter() => instance;
  static const instance = DrawingReflection._(
    modelName: r'Drawing',
    className: r'Drawing',
    xml: XmlReflection(
),
    mainShapePart: PropertyReflection<Drawing, UndefinedWrapper<
            Shape
>>(
      dartName: r'mainShape',
      nullable: false,
      required: false,
      oasName: r'mainShape',
      oasType: r'Shape',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_mainShapeGetter),
      setter: FunctionWrapper2(_mainShapeSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Shape.$reflection
        
,
)
),
    ),
    shapeOrNullPart: PropertyReflection<Drawing, UndefinedWrapper<
            ShapeOrNull
>>(
      dartName: r'shapeOrNull',
      nullable: false,
      required: false,
      oasName: r'shapeOrNull',
      oasType: r'ShapeOrNull',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_shapeOrNullGetter),
      setter: FunctionWrapper2(_shapeOrNullSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                ShapeOrNull.$reflection
        
,
)
),
    ),
    nullableShapePart: PropertyReflection<Drawing, UndefinedWrapper<
            NullableShape
?>>(
      dartName: r'nullableShape',
      nullable: true,
      required: false,
      oasName: r'nullableShape',
      oasType: r'NullableShape',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_nullableShapeGetter),
      setter: FunctionWrapper2(_nullableShapeSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                NullableShape.$reflection
        
),
)
),
    ),
    shapesPart: PropertyReflection<Drawing, UndefinedWrapper<
    List<
        
            Shape
>
>>(
      dartName: r'shapes',
      nullable: false,
      required: false,
      oasName: r'shapes',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_shapesGetter),
      setter: FunctionWrapper2(_shapesSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Shape.$reflection
        
,
)
)
,
)
),
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Fruit.$reflection
        
,
)
,
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const DrawingReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.mainShapePart,
    required this.shapeOrNullPart,
    required this.nullableShapePart,
    required this.shapesPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Drawing, UndefinedWrapper<
            Shape
>> mainShapePart;
  static UndefinedWrapper<
            Shape
> _mainShapeGetter(Drawing parent) {
    return parent.mainShape;
  }
  static void _mainShapeSetter(Drawing parent, UndefinedWrapper<
            Shape
> value) {
    parent.mainShape = value;
  }

  final PropertyReflection<Drawing, UndefinedWrapper<
            ShapeOrNull
>> shapeOrNullPart;
  static UndefinedWrapper<
            ShapeOrNull
> _shapeOrNullGetter(Drawing parent) {
    return parent.shapeOrNull;
  }
  static void _shapeOrNullSetter(Drawing parent, UndefinedWrapper<
            ShapeOrNull
> value) {
    parent.shapeOrNull = value;
  }

  final PropertyReflection<Drawing, UndefinedWrapper<
            NullableShape
?>> nullableShapePart;
  static UndefinedWrapper<
            NullableShape
?> _nullableShapeGetter(Drawing parent) {
    return parent.nullableShape;
  }
  static void _nullableShapeSetter(Drawing parent, UndefinedWrapper<
            NullableShape
?> value) {
    parent.nullableShape = value;
  }

  final PropertyReflection<Drawing, UndefinedWrapper<
    List<
        
            Shape
>
>> shapesPart;
  static UndefinedWrapper<
    List<
        
            Shape
>
> _shapesGetter(Drawing parent) {
    return parent.shapes;
  }
  static void _shapesSetter(Drawing parent, UndefinedWrapper<
    List<
        
            Shape
>
> value) {
    parent.shapes = value;
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
  List<PropertyReflection<Drawing, dynamic>> get properties => [
    mainShapePart,
shapeOrNullPart,
nullableShapePart,
shapesPart,
  ];

  @override
  final AdditionalPropertiesPart<Drawing, 
            Fruit
>? additionalPropertiesPart;

  static AdditionalProperties<
            Fruit
> _AdditionalPropertiesGetter(Drawing instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Drawing instance, AdditionalProperties<
            Fruit
> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Drawing, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Drawing empty() {
    return Drawing(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is DrawingReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}



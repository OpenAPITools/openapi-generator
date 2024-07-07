// Model reflection

part of 'drawing.dart';


//class reflection

class DrawingReflection extends ClassReflection<Drawing> {
  static DrawingReflection instanceGetter() => instance;
  static const instance = DrawingReflection._(
    modelName: r'Drawing',
    className: r'Drawing',
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
      classReflection: ShapeReflection.instance,
      getter: _mainShapeGetter,
      setter: _mainShapeSetter,
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
      classReflection: ShapeOrNullReflection.instance,
      getter: _shapeOrNullGetter,
      setter: _shapeOrNullSetter,
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
      classReflection: NullableShapeReflection.instance,
      getter: _nullableShapeGetter,
      setter: _nullableShapeSetter,
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
      itemsReflection: ItemsReflection<Drawing, 
            Shape
>(parentReflectionGetter: instanceGetter,classReflection: ShapeReflection.instance,),
      getter: _shapesGetter,
      setter: _shapesSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Drawing, 
            Fruit
>(parentReflectionGetter: instanceGetter,classReflection: FruitReflection.instance,),
      classReflection: FruitReflection.instance,
    ),
  );
  const DrawingReflection._({
    required this.modelName,
    required this.className,
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
  final Map<String, ClassReflection> discriminatorMappings;
  @override
  final Map<String, ClassReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;


  @override
  List<PropertyReflection<Drawing, dynamic>> get properties => [
    mainShapePart,
shapeOrNullPart,
nullableShapePart,
shapesPart,
  ];

  final AdditionalPropertiesReflection<Drawing, 
            Fruit
> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Drawing, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Drawing, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Drawing.canDeserialize(src);
  @override
  Drawing Function(Object? src) get deserializeFunction =>
      (src) => Drawing.deserialize(src);

  @override
  Object? Function(Drawing src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Drawing.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Drawing example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Drawing(
      mainShape: () {
        PartReflection? _partReflection = _reflection.mainShapePart;
        
        return UndefinedWrapper(


            
            


    Shape.$reflection.example()
    


);
      }(),
      shapeOrNull: () {
        PartReflection? _partReflection = _reflection.shapeOrNullPart;
        
        return UndefinedWrapper(


            
            


    ShapeOrNull.$reflection.example()
    


);
      }(),
      nullableShape: () {
        PartReflection? _partReflection = _reflection.nullableShapePart;
        
        return UndefinedWrapper(exampleNullable(() =>


            
            


    NullableShape.$reflection.example()
    


 ) );
      }(),
      shapes: () {
        PartReflection? _partReflection = _reflection.shapesPart;
        
        return UndefinedWrapper(


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    Shape.$reflection.example()
    


; })



);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => 


            
            


    Fruit.$reflection.example()
    


)); }(),
      
    );
  }
}

class DrawingXmlReflection {
    const DrawingXmlReflection();
}


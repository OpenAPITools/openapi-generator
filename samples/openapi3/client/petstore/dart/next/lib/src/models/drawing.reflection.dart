// Model reflection

part of 'drawing.dart';


//class reflection

class DrawingReflection extends ClassReflection<Drawing> {
  static const instance = DrawingReflection._(
    mainShape: PropertyReflection(
      dartName: r'mainShape',
      nullable: false,
      required: false,
      oasName: r'mainShape',
      oasType: r'Shape',
      pattern: null,
    ),
    shapeOrNull: PropertyReflection(
      dartName: r'shapeOrNull',
      nullable: true,
      required: false,
      oasName: r'shapeOrNull',
      oasType: r'ShapeOrNull',
      pattern: null,
    ),
    nullableShape: PropertyReflection(
      dartName: r'nullableShape',
      nullable: true,
      required: false,
      oasName: r'nullableShape',
      oasType: r'NullableShape',
      pattern: null,
    ),
    shapes: PropertyReflection(
      dartName: r'shapes',
      nullable: false,
      required: false,
      oasName: r'shapes',
      oasType: r'array',
      pattern: null,
    ),
  );
  const DrawingReflection._({
    required this.mainShape,
  
    required this.shapeOrNull,
  
    required this.nullableShape,
  
    required this.shapes,
  });

  final PropertyReflection<UndefinedWrapper<
            Shape
>> mainShape;
  final PropertyReflection<UndefinedWrapper<
            ShapeOrNull
?>> shapeOrNull;
  final PropertyReflection<UndefinedWrapper<
            NullableShape
?>> nullableShape;
  final PropertyReflection<UndefinedWrapper<
    List<
        
            Shape
>
>> shapes;

  @override
  List<PropertyReflection> get members => [
    mainShape,
shapeOrNull,
nullableShape,
shapes,
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
}

class DrawingXmlReflection {
    const DrawingXmlReflection();
}


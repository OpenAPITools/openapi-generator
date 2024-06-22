// Model reflection

part of 'isosceles_triangle.dart';


//class reflection

class IsoscelesTriangleReflection extends ClassReflection<IsoscelesTriangle> {
  static const instance = IsoscelesTriangleReflection._(
    shapeType: PropertyReflection(
      dartName: r'shapeType',
      nullable: false,
      required: true,
      oasName: r'shapeType',
      oasType: r'string',
      pattern: null,
    ),
    triangleType: PropertyReflection(
      dartName: r'triangleType',
      nullable: false,
      required: true,
      oasName: r'triangleType',
      oasType: r'string',
      pattern: null,
    ),
  );
  const IsoscelesTriangleReflection._({
    required this.shapeType,
  
    required this.triangleType,
  });

  final PropertyReflection<
            String
> shapeType;
  final PropertyReflection<
            String
> triangleType;

  @override
  List<PropertyReflection> get members => [
    shapeType,
triangleType,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => IsoscelesTriangle.canDeserialize(src);
  @override
  IsoscelesTriangle Function(Object? src) get deserializeFunction =>
      (src) => IsoscelesTriangle.deserialize(src);

  @override
  Object? Function(IsoscelesTriangle src) get serializeFunction =>
      (src) => src.serialize();
}

class IsoscelesTriangleXmlReflection {
    const IsoscelesTriangleXmlReflection();
}


// Model reflection

part of 'equilateral_triangle.dart';


//class reflection

class EquilateralTriangleReflection extends ClassReflection<EquilateralTriangle> {
  static const instance = EquilateralTriangleReflection._(
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
  const EquilateralTriangleReflection._({
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
    (src) => EquilateralTriangle.canDeserialize(src);
  @override
  EquilateralTriangle Function(Object? src) get deserializeFunction =>
      (src) => EquilateralTriangle.deserialize(src);

  @override
  Object? Function(EquilateralTriangle src) get serializeFunction =>
      (src) => src.serialize();
}

class EquilateralTriangleXmlReflection {
    const EquilateralTriangleXmlReflection();
}


// Model reflection

part of 'scalene_triangle.dart';


//class reflection

class ScaleneTriangleReflection extends ClassReflection<ScaleneTriangle> {
  static const instance = ScaleneTriangleReflection._(
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
  const ScaleneTriangleReflection._({
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
    (src) => ScaleneTriangle.canDeserialize(src);
  @override
  ScaleneTriangle Function(Object? src) get deserializeFunction =>
      (src) => ScaleneTriangle.deserialize(src);

  @override
  Object? Function(ScaleneTriangle src) get serializeFunction =>
      (src) => src.serialize();
}

class ScaleneTriangleXmlReflection {
    const ScaleneTriangleXmlReflection();
}


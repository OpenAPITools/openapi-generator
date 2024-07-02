// Model reflection

part of 'simple_quadrilateral.dart';


//class reflection

class SimpleQuadrilateralReflection extends ClassReflection<SimpleQuadrilateral> {
  static const instance = SimpleQuadrilateralReflection._(
    quadrilateralType: PropertyReflection(
      dartName: r'quadrilateralType',
      nullable: false,
      required: true,
      oasName: r'quadrilateralType',
      oasType: r'string',
      pattern: null,
    ),
    shapeType: PropertyReflection(
      dartName: r'shapeType',
      nullable: false,
      required: true,
      oasName: r'shapeType',
      oasType: r'string',
      pattern: null,
    ),
  );
  const SimpleQuadrilateralReflection._({
    required this.quadrilateralType,
  
    required this.shapeType,
  });

  final PropertyReflection<
            String
> quadrilateralType;
  final PropertyReflection<
            String
> shapeType;

  @override
  List<PropertyReflection> get members => [
    quadrilateralType,
shapeType,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => SimpleQuadrilateral.canDeserialize(src);
  @override
  SimpleQuadrilateral Function(Object? src) get deserializeFunction =>
      (src) => SimpleQuadrilateral.deserialize(src);

  @override
  Object? Function(SimpleQuadrilateral src) get serializeFunction =>
      (src) => src.serialize();
}

class SimpleQuadrilateralXmlReflection {
    const SimpleQuadrilateralXmlReflection();
}


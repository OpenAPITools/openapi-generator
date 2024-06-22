// Model reflection

part of 'complex_quadrilateral.dart';


//class reflection

class ComplexQuadrilateralReflection extends ClassReflection<ComplexQuadrilateral> {
  static const instance = ComplexQuadrilateralReflection._(
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
  const ComplexQuadrilateralReflection._({
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
    (src) => ComplexQuadrilateral.canDeserialize(src);
  @override
  ComplexQuadrilateral Function(Object? src) get deserializeFunction =>
      (src) => ComplexQuadrilateral.deserialize(src);

  @override
  Object? Function(ComplexQuadrilateral src) get serializeFunction =>
      (src) => src.serialize();
}

class ComplexQuadrilateralXmlReflection {
    const ComplexQuadrilateralXmlReflection();
}


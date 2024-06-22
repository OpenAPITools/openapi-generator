// Model reflection

part of 'quadrilateral_interface.dart';


//class reflection

class QuadrilateralInterfaceReflection extends ClassReflection<QuadrilateralInterface> {
  static const instance = QuadrilateralInterfaceReflection._(
    quadrilateralType: PropertyReflection(
      dartName: r'quadrilateralType',
      nullable: false,
      required: true,
      oasName: r'quadrilateralType',
      oasType: r'string',
      pattern: null,
    ),
  );
  const QuadrilateralInterfaceReflection._({
    required this.quadrilateralType,
  });

  final PropertyReflection<
            String
> quadrilateralType;

  @override
  List<PropertyReflection> get members => [
    quadrilateralType,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => QuadrilateralInterface.canDeserialize(src);
  @override
  QuadrilateralInterface Function(Object? src) get deserializeFunction =>
      (src) => QuadrilateralInterface.deserialize(src);

  @override
  Object? Function(QuadrilateralInterface src) get serializeFunction =>
      (src) => src.serialize();
}

class QuadrilateralInterfaceXmlReflection {
    const QuadrilateralInterfaceXmlReflection();
}


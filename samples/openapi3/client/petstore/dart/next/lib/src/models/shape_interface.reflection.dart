// Model reflection

part of 'shape_interface.dart';


//class reflection

class ShapeInterfaceReflection extends ClassReflection<ShapeInterface> {
  static const instance = ShapeInterfaceReflection._(
    shapeType: PropertyReflection(
      dartName: r'shapeType',
      nullable: false,
      required: true,
      oasName: r'shapeType',
      oasType: r'string',
      pattern: null,
    ),
  );
  const ShapeInterfaceReflection._({
    required this.shapeType,
  });

  final PropertyReflection<
            String
> shapeType;

  @override
  List<PropertyReflection> get members => [
    shapeType,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ShapeInterface.canDeserialize(src);
  @override
  ShapeInterface Function(Object? src) get deserializeFunction =>
      (src) => ShapeInterface.deserialize(src);

  @override
  Object? Function(ShapeInterface src) get serializeFunction =>
      (src) => src.serialize();
}

class ShapeInterfaceXmlReflection {
    const ShapeInterfaceXmlReflection();
}


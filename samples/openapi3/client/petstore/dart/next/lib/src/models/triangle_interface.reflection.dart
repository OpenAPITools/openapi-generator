// Model reflection

part of 'triangle_interface.dart';


//class reflection

class TriangleInterfaceReflection extends ClassReflection<TriangleInterface> {
  static const instance = TriangleInterfaceReflection._(
    triangleType: PropertyReflection(
      dartName: r'triangleType',
      nullable: false,
      required: true,
      oasName: r'triangleType',
      oasType: r'string',
      pattern: null,
    ),
  );
  const TriangleInterfaceReflection._({
    required this.triangleType,
  });

  final PropertyReflection<
            String
> triangleType;

  @override
  List<PropertyReflection> get members => [
    triangleType,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => TriangleInterface.canDeserialize(src);
  @override
  TriangleInterface Function(Object? src) get deserializeFunction =>
      (src) => TriangleInterface.deserialize(src);

  @override
  Object? Function(TriangleInterface src) get serializeFunction =>
      (src) => src.serialize();
}

class TriangleInterfaceXmlReflection {
    const TriangleInterfaceXmlReflection();
}


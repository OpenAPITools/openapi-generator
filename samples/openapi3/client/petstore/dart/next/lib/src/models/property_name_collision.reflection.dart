// Model reflection

part of 'property_name_collision.dart';


//class reflection

class PropertyNameCollisionReflection extends ClassReflection<PropertyNameCollision> {
  static const instance = PropertyNameCollisionReflection._(
    $type: PropertyReflection(
      dartName: r'$type',
      nullable: false,
      required: false,
      oasName: r'_type',
      oasType: r'string',
      pattern: null,
    ),
    type: PropertyReflection(
      dartName: r'type',
      nullable: false,
      required: false,
      oasName: r'type',
      oasType: r'string',
      pattern: null,
    ),
    type$: PropertyReflection(
      dartName: r'type$',
      nullable: false,
      required: false,
      oasName: r'type_',
      oasType: r'string',
      pattern: null,
    ),
  );
  const PropertyNameCollisionReflection._({
    required this.$type,
  
    required this.type,
  
    required this.type$,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> $type;
  final PropertyReflection<UndefinedWrapper<
            String
>> type;
  final PropertyReflection<UndefinedWrapper<
            String
>> type$;

  @override
  List<PropertyReflection> get members => [
    $type,
type,
type$,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => PropertyNameCollision.canDeserialize(src);
  @override
  PropertyNameCollision Function(Object? src) get deserializeFunction =>
      (src) => PropertyNameCollision.deserialize(src);

  @override
  Object? Function(PropertyNameCollision src) get serializeFunction =>
      (src) => src.serialize();
}

class PropertyNameCollisionXmlReflection {
    const PropertyNameCollisionXmlReflection();
}


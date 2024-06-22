// Model reflection

part of 'object_with_deprecated_fields.dart';


//class reflection

class ObjectWithDeprecatedFieldsReflection extends ClassReflection<ObjectWithDeprecatedFields> {
  static const instance = ObjectWithDeprecatedFieldsReflection._(
    uuid: PropertyReflection(
      dartName: r'uuid',
      nullable: false,
      required: false,
      oasName: r'uuid',
      oasType: r'string',
      pattern: null,
    ),
    id: PropertyReflection(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'number',
      pattern: null,
    ),
    deprecatedRef: PropertyReflection(
      dartName: r'deprecatedRef',
      nullable: false,
      required: false,
      oasName: r'deprecatedRef',
      oasType: r'DeprecatedObject',
      pattern: null,
    ),
    bars: PropertyReflection(
      dartName: r'bars',
      nullable: false,
      required: false,
      oasName: r'bars',
      oasType: r'array',
      pattern: null,
    ),
  );
  const ObjectWithDeprecatedFieldsReflection._({
    required this.uuid,
  
    required this.id,
  
    required this.deprecatedRef,
  
    required this.bars,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> uuid;
  final PropertyReflection<UndefinedWrapper<
            num
>> id;
  final PropertyReflection<UndefinedWrapper<
            DeprecatedObject
>> deprecatedRef;
  final PropertyReflection<UndefinedWrapper<
    List<
        
            String
>
>> bars;

  @override
  List<PropertyReflection> get members => [
    uuid,
id,
deprecatedRef,
bars,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ObjectWithDeprecatedFields.canDeserialize(src);
  @override
  ObjectWithDeprecatedFields Function(Object? src) get deserializeFunction =>
      (src) => ObjectWithDeprecatedFields.deserialize(src);

  @override
  Object? Function(ObjectWithDeprecatedFields src) get serializeFunction =>
      (src) => src.serialize();
}

class ObjectWithDeprecatedFieldsXmlReflection {
    const ObjectWithDeprecatedFieldsXmlReflection();
}


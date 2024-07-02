// Model reflection

part of 'tag.dart';


//class reflection

class TagReflection extends ClassReflection<Tag> {
  static const instance = TagReflection._(
    id: PropertyReflection(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'integer',
      pattern: null,
    ),
    name: PropertyReflection(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
    ),
  );
  const TagReflection._({
    required this.id,
  
    required this.name,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> id;
  final PropertyReflection<UndefinedWrapper<
            String
>> name;

  @override
  List<PropertyReflection> get members => [
    id,
name,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Tag.canDeserialize(src);
  @override
  Tag Function(Object? src) get deserializeFunction =>
      (src) => Tag.deserialize(src);

  @override
  Object? Function(Tag src) get serializeFunction =>
      (src) => src.serialize();
}

class TagXmlReflection {
    const TagXmlReflection();
}


// Model reflection

part of 'name.dart';


//class reflection

class NameReflection extends ClassReflection<Name> {
  static const instance = NameReflection._(
    name: PropertyReflection(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'integer',
      pattern: null,
    ),
    snakeCase: PropertyReflection(
      dartName: r'snakeCase',
      nullable: false,
      required: false,
      oasName: r'snake_case',
      oasType: r'integer',
      pattern: null,
    ),
    property: PropertyReflection(
      dartName: r'property',
      nullable: false,
      required: false,
      oasName: r'property',
      oasType: r'string',
      pattern: null,
    ),
    $123number: PropertyReflection(
      dartName: r'$123number',
      nullable: false,
      required: false,
      oasName: r'123Number',
      oasType: r'integer',
      pattern: null,
    ),
  );
  const NameReflection._({
    required this.name,
  
    required this.snakeCase,
  
    required this.property,
  
    required this.$123number,
  });

  final PropertyReflection<
            int
> name;
  final PropertyReflection<UndefinedWrapper<
            int
>> snakeCase;
  final PropertyReflection<UndefinedWrapper<
            String
>> property;
  final PropertyReflection<UndefinedWrapper<
            int
>> $123number;

  @override
  List<PropertyReflection> get members => [
    name,
snakeCase,
property,
$123number,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Name.canDeserialize(src);
  @override
  Name Function(Object? src) get deserializeFunction =>
      (src) => Name.deserialize(src);

  @override
  Object? Function(Name src) get serializeFunction =>
      (src) => src.serialize();
}

class NameXmlReflection {
    const NameXmlReflection();
}


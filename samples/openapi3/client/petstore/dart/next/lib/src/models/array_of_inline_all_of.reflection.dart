// Model reflection

part of 'array_of_inline_all_of.dart';


//class reflection

class ArrayOfInlineAllOfReflection extends ClassReflection<ArrayOfInlineAllOf> {
  static const instance = ArrayOfInlineAllOfReflection._(
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
      required: true,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
    ),
    arrayAllofDogProperty: PropertyReflection(
      dartName: r'arrayAllofDogProperty',
      nullable: false,
      required: false,
      oasName: r'array_allof_dog_property',
      oasType: r'array',
      pattern: null,
    ),
  );
  const ArrayOfInlineAllOfReflection._({
    required this.id,
  
    required this.name,
  
    required this.arrayAllofDogProperty,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> id;
  final PropertyReflection<
            String
> name;
  final PropertyReflection<UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner
>
>> arrayAllofDogProperty;

  @override
  List<PropertyReflection> get members => [
    id,
name,
arrayAllofDogProperty,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayOfInlineAllOf.canDeserialize(src);
  @override
  ArrayOfInlineAllOf Function(Object? src) get deserializeFunction =>
      (src) => ArrayOfInlineAllOf.deserialize(src);

  @override
  Object? Function(ArrayOfInlineAllOf src) get serializeFunction =>
      (src) => src.serialize();
}

class ArrayOfInlineAllOfXmlReflection {
    const ArrayOfInlineAllOfXmlReflection();
}


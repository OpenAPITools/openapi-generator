// Model reflection

part of 'array_of_inline_all_of_array_allof_dog_property_inner.dart';


//class reflection

class ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection extends ClassReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner> {
  static const instance = ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection._(
    breed: PropertyReflection(
      dartName: r'breed',
      nullable: false,
      required: false,
      oasName: r'breed',
      oasType: r'string',
      pattern: null,
    ),
    color: PropertyReflection(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
    ),
  );
  const ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection._({
    required this.breed,
  
    required this.color,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> breed;
  final PropertyReflection<UndefinedWrapper<
            String
>> color;

  @override
  List<PropertyReflection> get members => [
    breed,
color,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayOfInlineAllOfArrayAllofDogPropertyInner.canDeserialize(src);
  @override
  ArrayOfInlineAllOfArrayAllofDogPropertyInner Function(Object? src) get deserializeFunction =>
      (src) => ArrayOfInlineAllOfArrayAllofDogPropertyInner.deserialize(src);

  @override
  Object? Function(ArrayOfInlineAllOfArrayAllofDogPropertyInner src) get serializeFunction =>
      (src) => src.serialize();
}

class ArrayOfInlineAllOfArrayAllofDogPropertyInnerXmlReflection {
    const ArrayOfInlineAllOfArrayAllofDogPropertyInnerXmlReflection();
}


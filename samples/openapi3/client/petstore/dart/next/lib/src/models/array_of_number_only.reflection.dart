// Model reflection

part of 'array_of_number_only.dart';


//class reflection

class ArrayOfNumberOnlyReflection extends ClassReflection<ArrayOfNumberOnly> {
  static const instance = ArrayOfNumberOnlyReflection._(
    arrayNumber: PropertyReflection(
      dartName: r'arrayNumber',
      nullable: false,
      required: false,
      oasName: r'ArrayNumber',
      oasType: r'array',
      pattern: null,
    ),
  );
  const ArrayOfNumberOnlyReflection._({
    required this.arrayNumber,
  });

  final PropertyReflection<UndefinedWrapper<
    List<
        
            num
>
>> arrayNumber;

  @override
  List<PropertyReflection> get members => [
    arrayNumber,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayOfNumberOnly.canDeserialize(src);
  @override
  ArrayOfNumberOnly Function(Object? src) get deserializeFunction =>
      (src) => ArrayOfNumberOnly.deserialize(src);

  @override
  Object? Function(ArrayOfNumberOnly src) get serializeFunction =>
      (src) => src.serialize();
}

class ArrayOfNumberOnlyXmlReflection {
    const ArrayOfNumberOnlyXmlReflection();
}


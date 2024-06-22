// Model reflection

part of 'array_of_array_of_number_only.dart';


//class reflection

class ArrayOfArrayOfNumberOnlyReflection extends ClassReflection<ArrayOfArrayOfNumberOnly> {
  static const instance = ArrayOfArrayOfNumberOnlyReflection._(
    arrayArrayNumber: PropertyReflection(
      dartName: r'arrayArrayNumber',
      nullable: false,
      required: false,
      oasName: r'ArrayArrayNumber',
      oasType: r'array',
      pattern: null,
    ),
  );
  const ArrayOfArrayOfNumberOnlyReflection._({
    required this.arrayArrayNumber,
  });

  final PropertyReflection<UndefinedWrapper<
    List<
        
    List<
        
            num
>
>
>> arrayArrayNumber;

  @override
  List<PropertyReflection> get members => [
    arrayArrayNumber,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayOfArrayOfNumberOnly.canDeserialize(src);
  @override
  ArrayOfArrayOfNumberOnly Function(Object? src) get deserializeFunction =>
      (src) => ArrayOfArrayOfNumberOnly.deserialize(src);

  @override
  Object? Function(ArrayOfArrayOfNumberOnly src) get serializeFunction =>
      (src) => src.serialize();
}

class ArrayOfArrayOfNumberOnlyXmlReflection {
    const ArrayOfArrayOfNumberOnlyXmlReflection();
}


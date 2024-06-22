// Model reflection

part of 'number_only.dart';


//class reflection

class NumberOnlyReflection extends ClassReflection<NumberOnly> {
  static const instance = NumberOnlyReflection._(
    justNumber: PropertyReflection(
      dartName: r'justNumber',
      nullable: false,
      required: false,
      oasName: r'JustNumber',
      oasType: r'number',
      pattern: null,
    ),
  );
  const NumberOnlyReflection._({
    required this.justNumber,
  });

  final PropertyReflection<UndefinedWrapper<
            num
>> justNumber;

  @override
  List<PropertyReflection> get members => [
    justNumber,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => NumberOnly.canDeserialize(src);
  @override
  NumberOnly Function(Object? src) get deserializeFunction =>
      (src) => NumberOnly.deserialize(src);

  @override
  Object? Function(NumberOnly src) get serializeFunction =>
      (src) => src.serialize();
}

class NumberOnlyXmlReflection {
    const NumberOnlyXmlReflection();
}


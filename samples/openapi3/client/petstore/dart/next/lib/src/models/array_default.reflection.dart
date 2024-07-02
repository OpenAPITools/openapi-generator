// Model reflection

part of 'array_default.dart';


//class reflection

class ArrayDefaultReflection extends ClassReflection<ArrayDefault> {
  static const instance = ArrayDefaultReflection._(
    withDefaultEmptyBracket: PropertyReflection(
      dartName: r'withDefaultEmptyBracket',
      nullable: false,
      required: false,
      oasName: r'WithDefaultEmptyBracket',
      oasType: r'array',
      pattern: null,
    ),
    withoutDefault: PropertyReflection(
      dartName: r'withoutDefault',
      nullable: false,
      required: false,
      oasName: r'WithoutDefault',
      oasType: r'array',
      pattern: null,
    ),
  );
  const ArrayDefaultReflection._({
    required this.withDefaultEmptyBracket,
  
    required this.withoutDefault,
  });

  final PropertyReflection<UndefinedWrapper<
    List<
        
            String
>
>> withDefaultEmptyBracket;
  final PropertyReflection<UndefinedWrapper<
    List<
        
            String
>
>> withoutDefault;

  @override
  List<PropertyReflection> get members => [
    withDefaultEmptyBracket,
withoutDefault,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayDefault.canDeserialize(src);
  @override
  ArrayDefault Function(Object? src) get deserializeFunction =>
      (src) => ArrayDefault.deserialize(src);

  @override
  Object? Function(ArrayDefault src) get serializeFunction =>
      (src) => src.serialize();
}

class ArrayDefaultXmlReflection {
    const ArrayDefaultXmlReflection();
}


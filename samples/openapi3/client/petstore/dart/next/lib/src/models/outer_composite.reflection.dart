// Model reflection

part of 'outer_composite.dart';


//class reflection

class OuterCompositeReflection extends ClassReflection<OuterComposite> {
  static const instance = OuterCompositeReflection._(
    myNumber: PropertyReflection(
      dartName: r'myNumber',
      nullable: false,
      required: false,
      oasName: r'my_number',
      oasType: r'number',
      pattern: null,
    ),
    myString: PropertyReflection(
      dartName: r'myString',
      nullable: false,
      required: false,
      oasName: r'my_string',
      oasType: r'string',
      pattern: null,
    ),
    myBoolean: PropertyReflection(
      dartName: r'myBoolean',
      nullable: false,
      required: false,
      oasName: r'my_boolean',
      oasType: r'boolean',
      pattern: null,
    ),
  );
  const OuterCompositeReflection._({
    required this.myNumber,
  
    required this.myString,
  
    required this.myBoolean,
  });

  final PropertyReflection<UndefinedWrapper<
            num
>> myNumber;
  final PropertyReflection<UndefinedWrapper<
            String
>> myString;
  final PropertyReflection<UndefinedWrapper<
            bool
>> myBoolean;

  @override
  List<PropertyReflection> get members => [
    myNumber,
myString,
myBoolean,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => OuterComposite.canDeserialize(src);
  @override
  OuterComposite Function(Object? src) get deserializeFunction =>
      (src) => OuterComposite.deserialize(src);

  @override
  Object? Function(OuterComposite src) get serializeFunction =>
      (src) => src.serialize();
}

class OuterCompositeXmlReflection {
    const OuterCompositeXmlReflection();
}


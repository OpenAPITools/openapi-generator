// Model reflection

part of '__return.dart';


//class reflection

class $ReturnReflection extends ClassReflection<$Return> {
  static const instance = $ReturnReflection._(
    $return: PropertyReflection(
      dartName: r'$return',
      nullable: false,
      required: false,
      oasName: r'return',
      oasType: r'integer',
      pattern: null,
    ),
  );
  const $ReturnReflection._({
    required this.$return,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> $return;

  @override
  List<PropertyReflection> get members => [
    $return,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => $Return.canDeserialize(src);
  @override
  $Return Function(Object? src) get deserializeFunction =>
      (src) => $Return.deserialize(src);

  @override
  Object? Function($Return src) get serializeFunction =>
      (src) => src.serialize();
}

class $ReturnXmlReflection {
    const $ReturnXmlReflection();
}


// Model reflection

part of '__list.dart';


//class reflection

class $ListReflection extends ClassReflection<$List> {
  static const instance = $ListReflection._(
    $123list: PropertyReflection(
      dartName: r'$123list',
      nullable: false,
      required: false,
      oasName: r'123-list',
      oasType: r'string',
      pattern: null,
    ),
  );
  const $ListReflection._({
    required this.$123list,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> $123list;

  @override
  List<PropertyReflection> get members => [
    $123list,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => $List.canDeserialize(src);
  @override
  $List Function(Object? src) get deserializeFunction =>
      (src) => $List.deserialize(src);

  @override
  Object? Function($List src) get serializeFunction =>
      (src) => src.serialize();
}

class $ListXmlReflection {
    const $ListXmlReflection();
}


// Model reflection

part of '__200_response.dart';


//class reflection

class $200ResponseReflection extends ClassReflection<$200Response> {
  static const instance = $200ResponseReflection._(
    name: PropertyReflection(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'integer',
      pattern: null,
    ),
    propertyClass: PropertyReflection(
      dartName: r'propertyClass',
      nullable: false,
      required: false,
      oasName: r'class',
      oasType: r'string',
      pattern: null,
    ),
  );
  const $200ResponseReflection._({
    required this.name,
  
    required this.propertyClass,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> name;
  final PropertyReflection<UndefinedWrapper<
            String
>> propertyClass;

  @override
  List<PropertyReflection> get members => [
    name,
propertyClass,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => $200Response.canDeserialize(src);
  @override
  $200Response Function(Object? src) get deserializeFunction =>
      (src) => $200Response.deserialize(src);

  @override
  Object? Function($200Response src) get serializeFunction =>
      (src) => src.serialize();
}

class $200ResponseXmlReflection {
    const $200ResponseXmlReflection();
}


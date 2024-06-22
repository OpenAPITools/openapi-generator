// Model reflection

part of 'apple_req.dart';


//class reflection

class AppleReqReflection extends ClassReflection<AppleReq> {
  static const instance = AppleReqReflection._(
    cultivar: PropertyReflection(
      dartName: r'cultivar',
      nullable: false,
      required: true,
      oasName: r'cultivar',
      oasType: r'string',
      pattern: null,
    ),
    mealy: PropertyReflection(
      dartName: r'mealy',
      nullable: false,
      required: false,
      oasName: r'mealy',
      oasType: r'boolean',
      pattern: null,
    ),
  );
  const AppleReqReflection._({
    required this.cultivar,
  
    required this.mealy,
  });

  final PropertyReflection<
            String
> cultivar;
  final PropertyReflection<UndefinedWrapper<
            bool
>> mealy;

  @override
  List<PropertyReflection> get members => [
    cultivar,
mealy,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AppleReq.canDeserialize(src);
  @override
  AppleReq Function(Object? src) get deserializeFunction =>
      (src) => AppleReq.deserialize(src);

  @override
  Object? Function(AppleReq src) get serializeFunction =>
      (src) => src.serialize();
}

class AppleReqXmlReflection {
    const AppleReqXmlReflection();
}


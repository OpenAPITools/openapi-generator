// Model reflection

part of 'banana_req.dart';


//class reflection

class BananaReqReflection extends ClassReflection<BananaReq> {
  static const instance = BananaReqReflection._(
    lengthCm: PropertyReflection(
      dartName: r'lengthCm',
      nullable: false,
      required: true,
      oasName: r'lengthCm',
      oasType: r'number',
      pattern: null,
    ),
    sweet: PropertyReflection(
      dartName: r'sweet',
      nullable: false,
      required: false,
      oasName: r'sweet',
      oasType: r'boolean',
      pattern: null,
    ),
  );
  const BananaReqReflection._({
    required this.lengthCm,
  
    required this.sweet,
  });

  final PropertyReflection<
            num
> lengthCm;
  final PropertyReflection<UndefinedWrapper<
            bool
>> sweet;

  @override
  List<PropertyReflection> get members => [
    lengthCm,
sweet,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => BananaReq.canDeserialize(src);
  @override
  BananaReq Function(Object? src) get deserializeFunction =>
      (src) => BananaReq.deserialize(src);

  @override
  Object? Function(BananaReq src) get serializeFunction =>
      (src) => src.serialize();
}

class BananaReqXmlReflection {
    const BananaReqXmlReflection();
}


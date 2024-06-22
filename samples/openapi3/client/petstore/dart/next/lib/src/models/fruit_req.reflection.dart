// Model reflection

part of 'fruit_req.dart';


//class reflection

class FruitReqReflection extends ClassReflection<FruitReq> {
  static const instance = FruitReqReflection._(
  );
  const FruitReqReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FruitReq.canDeserialize(src);
  @override
  FruitReq Function(Object? src) get deserializeFunction =>
      (src) => FruitReq.deserialize(src);

  @override
  Object? Function(FruitReq src) get serializeFunction =>
      (src) => src.serialize();
}

class FruitReqXmlReflection {
    const FruitReqXmlReflection();
}


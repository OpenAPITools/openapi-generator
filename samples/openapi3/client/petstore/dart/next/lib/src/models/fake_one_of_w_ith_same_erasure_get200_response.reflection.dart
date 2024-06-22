// Model reflection

part of 'fake_one_of_w_ith_same_erasure_get200_response.dart';


//class reflection

class FakeOneOfWIthSameErasureGet200ResponseReflection extends ClassReflection<FakeOneOfWIthSameErasureGet200Response> {
  static const instance = FakeOneOfWIthSameErasureGet200ResponseReflection._(
  );
  const FakeOneOfWIthSameErasureGet200ResponseReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FakeOneOfWIthSameErasureGet200Response.canDeserialize(src);
  @override
  FakeOneOfWIthSameErasureGet200Response Function(Object? src) get deserializeFunction =>
      (src) => FakeOneOfWIthSameErasureGet200Response.deserialize(src);

  @override
  Object? Function(FakeOneOfWIthSameErasureGet200Response src) get serializeFunction =>
      (src) => src.serialize();
}

class FakeOneOfWIthSameErasureGet200ResponseXmlReflection {
    const FakeOneOfWIthSameErasureGet200ResponseXmlReflection();
}


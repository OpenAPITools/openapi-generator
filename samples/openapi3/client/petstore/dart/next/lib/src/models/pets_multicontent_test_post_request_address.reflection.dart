// Model reflection

part of 'pets_multicontent_test_post_request_address.dart';


//class reflection

class PetsMulticontentTestPostRequestAddressReflection extends ClassReflection<PetsMulticontentTestPostRequestAddress> {
  static const instance = PetsMulticontentTestPostRequestAddressReflection._(
    street: PropertyReflection(
      dartName: r'street',
      nullable: false,
      required: false,
      oasName: r'street',
      oasType: r'string',
      pattern: null,
    ),
    city: PropertyReflection(
      dartName: r'city',
      nullable: false,
      required: false,
      oasName: r'city',
      oasType: r'string',
      pattern: null,
    ),
  );
  const PetsMulticontentTestPostRequestAddressReflection._({
    required this.street,
  
    required this.city,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> street;
  final PropertyReflection<UndefinedWrapper<
            String
>> city;

  @override
  List<PropertyReflection> get members => [
    street,
city,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => PetsMulticontentTestPostRequestAddress.canDeserialize(src);
  @override
  PetsMulticontentTestPostRequestAddress Function(Object? src) get deserializeFunction =>
      (src) => PetsMulticontentTestPostRequestAddress.deserialize(src);

  @override
  Object? Function(PetsMulticontentTestPostRequestAddress src) get serializeFunction =>
      (src) => src.serialize();
}

class PetsMulticontentTestPostRequestAddressXmlReflection {
    const PetsMulticontentTestPostRequestAddressXmlReflection();
}


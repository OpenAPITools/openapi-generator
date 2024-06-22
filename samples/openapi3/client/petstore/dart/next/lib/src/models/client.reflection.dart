// Model reflection

part of 'client.dart';


//class reflection

class ClientReflection extends ClassReflection<Client> {
  static const instance = ClientReflection._(
    client: PropertyReflection(
      dartName: r'client',
      nullable: false,
      required: false,
      oasName: r'client',
      oasType: r'string',
      pattern: null,
    ),
  );
  const ClientReflection._({
    required this.client,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> client;

  @override
  List<PropertyReflection> get members => [
    client,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Client.canDeserialize(src);
  @override
  Client Function(Object? src) get deserializeFunction =>
      (src) => Client.deserialize(src);

  @override
  Object? Function(Client src) get serializeFunction =>
      (src) => src.serialize();
}

class ClientXmlReflection {
    const ClientXmlReflection();
}


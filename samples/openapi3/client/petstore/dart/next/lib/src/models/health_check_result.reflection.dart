// Model reflection

part of 'health_check_result.dart';


//class reflection

class HealthCheckResultReflection extends ClassReflection<HealthCheckResult> {
  static const instance = HealthCheckResultReflection._(
    nullableMessage: PropertyReflection(
      dartName: r'nullableMessage',
      nullable: true,
      required: false,
      oasName: r'NullableMessage',
      oasType: r'string',
      pattern: null,
    ),
  );
  const HealthCheckResultReflection._({
    required this.nullableMessage,
  });

  final PropertyReflection<UndefinedWrapper<
            String
?>> nullableMessage;

  @override
  List<PropertyReflection> get members => [
    nullableMessage,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => HealthCheckResult.canDeserialize(src);
  @override
  HealthCheckResult Function(Object? src) get deserializeFunction =>
      (src) => HealthCheckResult.deserialize(src);

  @override
  Object? Function(HealthCheckResult src) get serializeFunction =>
      (src) => src.serialize();
}

class HealthCheckResultXmlReflection {
    const HealthCheckResultXmlReflection();
}


// Model reflection

part of 'api_response.dart';


//class reflection

class ApiResponseReflection extends ClassReflection<ApiResponse> {
  static const instance = ApiResponseReflection._(
    code: PropertyReflection(
      dartName: r'code',
      nullable: false,
      required: false,
      oasName: r'code',
      oasType: r'integer',
      pattern: null,
    ),
    type: PropertyReflection(
      dartName: r'type',
      nullable: false,
      required: false,
      oasName: r'type',
      oasType: r'string',
      pattern: null,
    ),
    message: PropertyReflection(
      dartName: r'message',
      nullable: false,
      required: false,
      oasName: r'message',
      oasType: r'string',
      pattern: null,
    ),
  );
  const ApiResponseReflection._({
    required this.code,
  
    required this.type,
  
    required this.message,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> code;
  final PropertyReflection<UndefinedWrapper<
            String
>> type;
  final PropertyReflection<UndefinedWrapper<
            String
>> message;

  @override
  List<PropertyReflection> get members => [
    code,
type,
message,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ApiResponse.canDeserialize(src);
  @override
  ApiResponse Function(Object? src) get deserializeFunction =>
      (src) => ApiResponse.deserialize(src);

  @override
  Object? Function(ApiResponse src) get serializeFunction =>
      (src) => src.serialize();
}

class ApiResponseXmlReflection {
    const ApiResponseXmlReflection();
}


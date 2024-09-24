// Model def

import 'package:petstore_api/_internal.dart';


part 'api_response.reflection.dart';


/// ApiResponseMixin
///
/// Properties:
/// * [code] 
/// * [type] 
/// * [message] 
mixin ApiResponseMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get code;
UndefinedWrapper<
            String
> get type;
UndefinedWrapper<
            String
> get message;
  
}

/// ApiResponse
///
/// Properties:
/// * [code] 
/// * [type] 
/// * [message] 
class ApiResponse with
$OpenApiObjectMixin,

ApiResponseMixin {
  @override
  UndefinedWrapper<
            int
> code;
  @override
  UndefinedWrapper<
            String
> type;
  @override
  UndefinedWrapper<
            String
> message;

  AdditionalProperties<Object
?> additionalProperties;

  

  ApiResponse.$all({
        required this.code,
    required this.type,
    required this.message,
    required this.additionalProperties,
    
  });

  ApiResponse({
      this.code = const UndefinedWrapper
        .undefined()
,
  this.type = const UndefinedWrapper
        .undefined()
,
  this.message = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ApiResponseReflection.instance;
  ApiResponseReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory ApiResponse.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ApiResponse clone() {
    return $reflection.clone(this);
  }
}












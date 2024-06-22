// Model def

import 'package:openapi/_internal.dart';


part 'api_response.reflection.dart';
part 'api_response.serialization.dart';


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

  

  

  ApiResponse.$all({
        required this.code,
    required this.type,
    required this.message,
    
    
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
    
    
  });

  static const $reflection = ApiResponseReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$ApiResponseToMap(this);
  }
  factory ApiResponse.fromMap(Map<String, dynamic> src) {
    return _$ApiResponseFromMap(src);
  }
  static ApiResponse? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ApiResponse.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ApiResponseCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ApiResponse.deserialize(Object? src) {
    return _$ApiResponseDeserialize(src);
  }
  static ApiResponse? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ApiResponse.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ApiResponseCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$ApiResponseSerialize(this);
  }
}





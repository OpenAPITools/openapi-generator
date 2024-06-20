// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'health_check_result.reflection.dart';
part 'health_check_result.serialization.dart';


//class defination

///
mixin HealthCheckResultMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String?> get nullableMessage;


}

///
class HealthCheckResult with
$OpenApiObjectMixin,


HealthCheckResultMixin {
  @override
  UndefinedWrapper<String?> nullableMessage;





  HealthCheckResult.$all({
    required this.nullableMessage,
    
    
  });

  HealthCheckResult({
    this.nullableMessage = const UndefinedWrapper.undefined(),
    
    
  });
}





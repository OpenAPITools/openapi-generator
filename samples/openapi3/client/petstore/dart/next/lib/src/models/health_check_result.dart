// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'health_check_result.reflection.dart';
part 'health_check_result.serialization.dart';


/// Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
///
/// Properties:
/// * [nullableMessage] 
mixin HealthCheckResultMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String?> get nullableMessage;

}

/// Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
///
/// Properties:
/// * [nullableMessage] 
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





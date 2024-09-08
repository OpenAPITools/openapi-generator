// Model def

import 'package:petstore_api/_internal.dart';


part 'client.reflection.dart';


/// ClientMixin
///
/// Properties:
/// * [client] 
mixin ClientMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get client;
  
}

/// Client
///
/// Properties:
/// * [client] 
class Client with
$OpenApiObjectMixin,

ClientMixin {
  @override
  UndefinedWrapper<
            String
> client;

  AdditionalProperties<Object
?> additionalProperties;

  

  Client.$all({
        required this.client,
    required this.additionalProperties,
    
  });

  Client({
      this.client = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ClientReflection.instance;
  ClientReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Client.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Client clone() {
    return $reflection.clone(this);
  }
}






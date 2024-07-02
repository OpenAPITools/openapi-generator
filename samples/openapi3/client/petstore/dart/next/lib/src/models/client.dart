// Model def

import 'package:openapi/_internal.dart';


part 'client.reflection.dart';
part 'client.serialization.dart';


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
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ClientReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$ClientToMap(this);
  }
  factory Client.fromMap(Map<String, dynamic> src) {
    return _$ClientFromMap(src);
  }
  static Client? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Client.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ClientCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Client.deserialize(Object? src) {
    return _$ClientDeserialize(src);
  }
  static Client? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Client.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ClientCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ClientSerialize(this);
  }
}





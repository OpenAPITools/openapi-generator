// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'client.dart';


//class serialization

Map<String, dynamic> _$ClientToMap(Client instance) {
  final _reflection = ClientReflection.instance;
  return <String, dynamic>{
    if (instance.client.isDefined)
    _reflection.clientPart.oasName: (
            String
 v) {
      return v;
    }(instance.client.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Client _$ClientFromMap(Map<String, dynamic> src) {
  const _reflection = ClientReflection.instance;
  return Client.$all(
    client: src.getOrUndefinedMapped(_reflection.clientPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$ClientCanFromMap(Map<String, dynamic> src) {
  final _reflection = ClientReflection.instance;

  if (!src.getOrUndefined(_reflection.clientPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.clientPart.required,
)) {
    return false;
  }
  if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }

  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Client _$ClientDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ClientFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ClientCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ClientCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$ClientSerialize(Client src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$ClientToXml(Client instance) {
  final reflection = ClientXmlReflection.instance;
  final result = XmlElement(
    XmlName(reflection.oasName, reflection.oasNamespace),
    //attributes
    [

    ],
    //elements
    [
    ],
  );
  return result;
}

Client _$ClientFromXml(XmlElement src) {
  final reflection = ClientXmlReflection.instance;
  return Client.$all(

  );
}
*/


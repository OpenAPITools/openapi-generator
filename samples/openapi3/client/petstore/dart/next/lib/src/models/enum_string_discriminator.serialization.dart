// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'enum_string_discriminator.dart';


//class serialization

Map<String, dynamic> _$EnumStringDiscriminatorToMap(EnumStringDiscriminator instance) {
  final _reflection = EnumStringDiscriminatorReflection.instance;
  return <String, dynamic>{
    
    _reflection.enumStrTypePart.oasName: (
            EnumStringDiscriminatorEnumStrTypeEnum

 v) {
      return v.value;
    }(instance.enumStrType),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

EnumStringDiscriminator _$EnumStringDiscriminatorFromMap(Map<String, dynamic> src) {
  const _reflection = EnumStringDiscriminatorReflection.instance;
  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  return EnumStringDiscriminator.$all(
    enumStrType: src.getRequiredMapped(_reflection.enumStrTypePart.oasName, (v) => 
(

            
                    EnumStringDiscriminatorEnumStrTypeEnum.$safe(( v is String ? v as String :




throwArgumentMismatch(String, v)

))

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$EnumStringDiscriminatorCanFromMap(Map<String, dynamic> src) {
  final _reflection = EnumStringDiscriminatorReflection.instance;

  if (!src.getOrUndefined(_reflection.enumStrTypePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
     && EnumStringDiscriminatorEnumStrTypeEnum.canDeserialize(v)
)
),
    unDefined: () => !_reflection.enumStrTypePart.required,
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
EnumStringDiscriminator _$EnumStringDiscriminatorDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$EnumStringDiscriminatorFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$EnumStringDiscriminatorCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$EnumStringDiscriminatorCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$EnumStringDiscriminatorSerialize(EnumStringDiscriminator src) {
  Map<String, dynamic> initialResult = () {
    
      return _$EnumStringDiscriminatorToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$EnumStringDiscriminatorToXml(EnumStringDiscriminator instance) {
  final reflection = EnumStringDiscriminatorXmlReflection.instance;
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

EnumStringDiscriminator _$EnumStringDiscriminatorFromXml(XmlElement src) {
  final reflection = EnumStringDiscriminatorXmlReflection.instance;
  return EnumStringDiscriminator.$all(

  );
}
*/


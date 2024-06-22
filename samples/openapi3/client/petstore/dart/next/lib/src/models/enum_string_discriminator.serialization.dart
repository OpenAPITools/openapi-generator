// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'enum_string_discriminator.dart';


//class serialization

Map<String, dynamic> _$EnumStringDiscriminatorToMap(EnumStringDiscriminator instance) {
  final _reflection = EnumStringDiscriminatorReflection.instance;
  return <String, dynamic>{
    
    _reflection.enumStrType.oasName: (
            EnumStringDiscriminatorEnumStrTypeEnum
 v) {
      return v.value;
    }(instance.enumStrType),
    
    
  };
}

EnumStringDiscriminator _$EnumStringDiscriminatorFromMap(Map<String, dynamic> src) {
  final _reflection = EnumStringDiscriminatorReflection.instance;
  return EnumStringDiscriminator.$all(
    enumStrType: src.getRequiredMapped(_reflection.enumStrType.oasName, (v) => 
(

    
            
                    EnumStringDiscriminatorEnumStrTypeEnum.$safe(v as String)
            

)


),
    
    
  );
}

bool _$EnumStringDiscriminatorCanFromMap(Map<String, dynamic> src) {
  final _reflection = EnumStringDiscriminatorReflection.instance;
  if (!src.getOrUndefined(_reflection.enumStrType.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.enumStrType.required,
)) {
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
Object? _$EnumStringDiscriminatorSerialize(EnumStringDiscriminator src) {
  
  return src.toMap();
  
  
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


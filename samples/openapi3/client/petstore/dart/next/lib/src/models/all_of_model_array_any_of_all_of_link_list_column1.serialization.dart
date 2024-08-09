// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'all_of_model_array_any_of_all_of_link_list_column1.dart';


//class serialization

Map<String, dynamic> _$AllOfModelArrayAnyOfAllOfLinkListColumn1ToMap(AllOfModelArrayAnyOfAllOfLinkListColumn1 instance) {
  final _reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection.instance;
  return <String, dynamic>{
    
    _reflection.valuePart.oasName: (
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value

>

 v) {
      return v.map((v) => v.serialize()).toList();
    }(instance.value),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

AllOfModelArrayAnyOfAllOfLinkListColumn1 _$AllOfModelArrayAnyOfAllOfLinkListColumn1FromMap(Map<String, dynamic> src) {
  const _reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection.instance;
  return AllOfModelArrayAnyOfAllOfLinkListColumn1.$all(
    value: src.getRequiredMapped(_reflection.valuePart.oasName, (v) => 
(

            
            v as List
            

)

.map((v) => AllOfModelArrayAnyOfAllOfLinkListColumn1Value.deserialize
(

            v

)


).toList()
),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$AllOfModelArrayAnyOfAllOfLinkListColumn1CanFromMap(Map<String, dynamic> src) {
  final _reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection.instance;

  if (!src.getOrUndefined(_reflection.valuePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value.canDeserialize(v)
            
))
),
    unDefined: () => !_reflection.valuePart.required,
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
AllOfModelArrayAnyOfAllOfLinkListColumn1 _$AllOfModelArrayAnyOfAllOfLinkListColumn1Deserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1FromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$AllOfModelArrayAnyOfAllOfLinkListColumn1CanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1CanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$AllOfModelArrayAnyOfAllOfLinkListColumn1Serialize(AllOfModelArrayAnyOfAllOfLinkListColumn1 src) {
  Map<String, dynamic> initialResult = () {
    
      return _$AllOfModelArrayAnyOfAllOfLinkListColumn1ToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$AllOfModelArrayAnyOfAllOfLinkListColumn1ToXml(AllOfModelArrayAnyOfAllOfLinkListColumn1 instance) {
  final reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1XmlReflection.instance;
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

AllOfModelArrayAnyOfAllOfLinkListColumn1 _$AllOfModelArrayAnyOfAllOfLinkListColumn1FromXml(XmlElement src) {
  final reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1XmlReflection.instance;
  return AllOfModelArrayAnyOfAllOfLinkListColumn1.$all(

  );
}
*/


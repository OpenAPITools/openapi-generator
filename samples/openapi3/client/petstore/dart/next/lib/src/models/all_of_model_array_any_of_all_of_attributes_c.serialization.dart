// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'all_of_model_array_any_of_all_of_attributes_c.dart';


//class serialization

Map<String, dynamic> _$AllOfModelArrayAnyOfAllOfAttributesCToMap(AllOfModelArrayAnyOfAllOfAttributesC instance) {
  final _reflection = AllOfModelArrayAnyOfAllOfAttributesCReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
    if (instance.oneOf1.isDefined) ...instance.oneOf1.valueRequired.toMap(),
    
  };
}

AllOfModelArrayAnyOfAllOfAttributesC _$AllOfModelArrayAnyOfAllOfAttributesCFromMap(Map<String, dynamic> src) {
  const _reflection = AllOfModelArrayAnyOfAllOfAttributesCReflection.instance;
  return AllOfModelArrayAnyOfAllOfAttributesC.$all(
        additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
    oneOf0: Pet.canDeserialize(src) ? UndefinedWrapper(Pet.deserialize(src)) :  UndefinedWrapper.undefined(),
    oneOf1: Order.canDeserialize(src) ? UndefinedWrapper(Order.deserialize(src)) :  UndefinedWrapper.undefined(),
  );
}

bool _$AllOfModelArrayAnyOfAllOfAttributesCCanFromMap(Map<String, dynamic> src) {
  final _reflection = AllOfModelArrayAnyOfAllOfAttributesCReflection.instance;

    if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }


  final oneOfs = [
    () => Pet.canDeserialize(src),
      () => Order.canDeserialize(src),
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
AllOfModelArrayAnyOfAllOfAttributesC _$AllOfModelArrayAnyOfAllOfAttributesCDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AllOfModelArrayAnyOfAllOfAttributesCFromMap(src);
  } else {
    
    final v = src;
    return AllOfModelArrayAnyOfAllOfAttributesC.$all(
      oneOf0: (v == null ? false :
(

    
            Pet.canDeserialize(v)
            
)) ? UndefinedWrapper(Pet.deserialize
(

            v

)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            Order.canDeserialize(v)
            
)) ? UndefinedWrapper(Order.deserialize
(

            v

)


) : UndefinedWrapper.undefined(),
      // Additional Properties only make sense if the src is a Map<String, dynamic>
      additionalProperties: AdditionalProperties(),
    );
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$AllOfModelArrayAnyOfAllOfAttributesCCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AllOfModelArrayAnyOfAllOfAttributesCCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            Pet.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            Order.canDeserialize(v)
            
),
    ];
    final validOneOfs = oneOfs.where((x) => x()).take(2).length;
    if (validOneOfs == 1) {
      return true;
    }
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$AllOfModelArrayAnyOfAllOfAttributesCSerialize(AllOfModelArrayAnyOfAllOfAttributesC src) {
  Object? initialResult = () {
    
    
    if (src.oneOf0.isDefined) {final v = src.oneOf0.valueRequired; return v.serialize(); }
    if (src.oneOf1.isDefined) {final v = src.oneOf1.valueRequired; return v.serialize(); }
    return null;
  }();
  if (initialResult is Map<String, Object?>) {
    return {
      ...src.additionalProperties,
      ...initialResult,
    };
  }
  return initialResult;
}


/*
XmlElement _$AllOfModelArrayAnyOfAllOfAttributesCToXml(AllOfModelArrayAnyOfAllOfAttributesC instance) {
  final reflection = AllOfModelArrayAnyOfAllOfAttributesCXmlReflection.instance;
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

AllOfModelArrayAnyOfAllOfAttributesC _$AllOfModelArrayAnyOfAllOfAttributesCFromXml(XmlElement src) {
  final reflection = AllOfModelArrayAnyOfAllOfAttributesCXmlReflection.instance;
  return AllOfModelArrayAnyOfAllOfAttributesC.$all(

  );
}
*/


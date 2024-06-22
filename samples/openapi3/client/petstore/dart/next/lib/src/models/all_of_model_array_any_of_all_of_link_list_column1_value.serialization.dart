// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'all_of_model_array_any_of_all_of_link_list_column1_value.dart';


//class serialization

Map<String, dynamic> _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueToMap(AllOfModelArrayAnyOfAllOfLinkListColumn1Value instance) {
  final _reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection.instance;
  return <String, dynamic>{
    
    
    if (instance.anyOf0.isDefined) ...instance.anyOf0.valueRequired.toMap(),
    if (instance.anyOf1.isDefined) ...instance.anyOf1.valueRequired.toMap(),
  };
}

AllOfModelArrayAnyOfAllOfLinkListColumn1Value _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueFromMap(Map<String, dynamic> src) {
  final _reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection.instance;
  return AllOfModelArrayAnyOfAllOfLinkListColumn1Value.$all(
        
    
    anyOf0: User.canDeserialize(src) ? UndefinedWrapper(User.deserialize(src)) :  UndefinedWrapper.undefined(),
    anyOf1: Tag.canDeserialize(src) ? UndefinedWrapper(Tag.deserialize(src)) :  UndefinedWrapper.undefined(),
  );
}

bool _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueCanFromMap(Map<String, dynamic> src) {
  final _reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection.instance;
    
  final anyOfs = [
    () => User.canDeserialize(src),
  
    () => Tag.canDeserialize(src),
  ];
  final validAnyOfs = anyOfs.where((x) => x()).take(1).length;
  if (validAnyOfs == 0) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
AllOfModelArrayAnyOfAllOfLinkListColumn1Value _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueFromMap(src);
  } else {
    
    final v = src;
    return AllOfModelArrayAnyOfAllOfLinkListColumn1Value.$all(

      anyOf0: (v == null ? false :
(

    
            User.canDeserialize(v)
            
)) ? UndefinedWrapper(User.deserialize
(

    
            v


)


) : UndefinedWrapper.undefined(),
      anyOf1: (v == null ? false :
(

    
            Tag.canDeserialize(v)
            
)) ? UndefinedWrapper(Tag.deserialize
(

    
            v


)


) : UndefinedWrapper.undefined(),
    );
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueCanFromMap(src);
  } else {
    final v = src;
    final anyOfs = [
      () => v == null ? false :
(

    
            User.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            Tag.canDeserialize(v)
            
),
    ];
    final validAnyOfs = anyOfs.where((x) => x()).take(1).length;
    if (validAnyOfs > 0) {
      return true;
    }
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueSerialize(AllOfModelArrayAnyOfAllOfLinkListColumn1Value src) {
  
  
  if (src.anyOf0.isDefined) {final v = src.anyOf0.valueRequired; return v.serialize(); }
  
  if (src.anyOf1.isDefined) {final v = src.anyOf1.valueRequired; return v.serialize(); }
  
  return null;
}


/*
XmlElement _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueToXml(AllOfModelArrayAnyOfAllOfLinkListColumn1Value instance) {
  final reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1ValueXmlReflection.instance;
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

AllOfModelArrayAnyOfAllOfLinkListColumn1Value _$AllOfModelArrayAnyOfAllOfLinkListColumn1ValueFromXml(XmlElement src) {
  final reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1ValueXmlReflection.instance;
  return AllOfModelArrayAnyOfAllOfLinkListColumn1Value.$all(

  );
}
*/


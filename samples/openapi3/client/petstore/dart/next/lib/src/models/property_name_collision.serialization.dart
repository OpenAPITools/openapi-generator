// Model serialization
part of 'property_name_collision.dart';


//class serialization

Map<String, dynamic> _$PropertyNameCollisionToJson(PropertyNameCollision instance) => <String, dynamic>{

};

PropertyNameCollision _$PropertyNameCollisionFromJson(Map<String, dynamic> src) {
  return PropertyNameCollision.$all(

  );
}

XmlElement _$PropertyNameCollisionToXml(PropertyNameCollision instance) {
  final reflection = PropertyNameCollisionXmlReflection.instance;
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

PropertyNameCollision _$PropertyNameCollisionFromXml(XmlElement src) {
  final reflection = PropertyNameCollisionXmlReflection.instance;
  return PropertyNameCollision.$all(

  );
}


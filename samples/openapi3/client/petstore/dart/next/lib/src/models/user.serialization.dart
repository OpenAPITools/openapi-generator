// Model serialization
part of 'user.dart';


//class serialization

Map<String, dynamic> _$UserToJson(User instance) => <String, dynamic>{

};

User _$UserFromJson(Map<String, dynamic> src) {
  return User.$all(

  );
}

XmlElement _$UserToXml(User instance) {
  final reflection = UserXmlReflection.instance;
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

User _$UserFromXml(XmlElement src) {
  final reflection = UserXmlReflection.instance;
  return User.$all(

  );
}


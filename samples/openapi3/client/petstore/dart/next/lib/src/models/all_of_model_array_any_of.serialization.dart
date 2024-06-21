// Model serialization
part of 'all_of_model_array_any_of.dart';


//class serialization

Map<String, dynamic> _$AllOfModelArrayAnyOfToJson(AllOfModelArrayAnyOf instance) => <String, dynamic>{

};

AllOfModelArrayAnyOf _$AllOfModelArrayAnyOfFromJson(Map<String, dynamic> src) {
  return AllOfModelArrayAnyOf.$all(

  );
}

XmlElement _$AllOfModelArrayAnyOfToXml(AllOfModelArrayAnyOf instance) {
  final reflection = AllOfModelArrayAnyOfXmlReflection.instance;
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

AllOfModelArrayAnyOf _$AllOfModelArrayAnyOfFromXml(XmlElement src) {
  final reflection = AllOfModelArrayAnyOfXmlReflection.instance;
  return AllOfModelArrayAnyOf.$all(

  );
}


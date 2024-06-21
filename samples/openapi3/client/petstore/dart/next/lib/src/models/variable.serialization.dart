// Model serialization
part of 'variable.dart';


//class serialization

Map<String, dynamic> _$VariableToJson(Variable instance) => <String, dynamic>{

};

Variable _$VariableFromJson(Map<String, dynamic> src) {
  return Variable.$all(

  );
}

XmlElement _$VariableToXml(Variable instance) {
  final reflection = VariableXmlReflection.instance;
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

Variable _$VariableFromXml(XmlElement src) {
  final reflection = VariableXmlReflection.instance;
  return Variable.$all(

  );
}


// Model serialization
part of 'drawing.dart';


//class serialization

Map<String, dynamic> _$DrawingToJson(Drawing instance) => <String, dynamic>{

};

Drawing _$DrawingFromJson(Map<String, dynamic> src) {
  return Drawing.$all(

  );
}

XmlElement _$DrawingToXml(Drawing instance) {
  final reflection = DrawingXmlReflection.instance;
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

Drawing _$DrawingFromXml(XmlElement src) {
  final reflection = DrawingXmlReflection.instance;
  return Drawing.$all(

  );
}


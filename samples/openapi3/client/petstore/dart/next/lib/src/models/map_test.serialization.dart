// Model serialization
part of 'map_test.dart';


//class serialization

Map<String, dynamic> _$MapTestToJson(MapTest instance) => <String, dynamic>{

};

MapTest _$MapTestFromJson(Map<String, dynamic> src) {
  return MapTest.$all(

  );
}

XmlElement _$MapTestToXml(MapTest instance) {
  final reflection = MapTestXmlReflection.instance;
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

MapTest _$MapTestFromXml(XmlElement src) {
  final reflection = MapTestXmlReflection.instance;
  return MapTest.$all(

  );
}


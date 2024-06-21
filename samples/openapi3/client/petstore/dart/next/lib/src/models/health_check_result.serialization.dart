// Model serialization
part of 'health_check_result.dart';


//class serialization

Map<String, dynamic> _$HealthCheckResultToJson(HealthCheckResult instance) => <String, dynamic>{

};

HealthCheckResult _$HealthCheckResultFromJson(Map<String, dynamic> src) {
  return HealthCheckResult.$all(

  );
}

XmlElement _$HealthCheckResultToXml(HealthCheckResult instance) {
  final reflection = HealthCheckResultXmlReflection.instance;
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

HealthCheckResult _$HealthCheckResultFromXml(XmlElement src) {
  final reflection = HealthCheckResultXmlReflection.instance;
  return HealthCheckResult.$all(

  );
}


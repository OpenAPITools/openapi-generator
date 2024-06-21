// Model serialization
part of 'upload_file_request.dart';


//class serialization

Map<String, dynamic> _$UploadFileRequestToJson(UploadFileRequest instance) => <String, dynamic>{

};

UploadFileRequest _$UploadFileRequestFromJson(Map<String, dynamic> src) {
  return UploadFileRequest.$all(

  );
}

XmlElement _$UploadFileRequestToXml(UploadFileRequest instance) {
  final reflection = UploadFileRequestXmlReflection.instance;
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

UploadFileRequest _$UploadFileRequestFromXml(XmlElement src) {
  final reflection = UploadFileRequestXmlReflection.instance;
  return UploadFileRequest.$all(

  );
}


// Model serialization
part of 'upload_file_with_required_file_request.dart';


//class serialization

Map<String, dynamic> _$UploadFileWithRequiredFileRequestToJson(UploadFileWithRequiredFileRequest instance) => <String, dynamic>{

};

UploadFileWithRequiredFileRequest _$UploadFileWithRequiredFileRequestFromJson(Map<String, dynamic> src) {
  return UploadFileWithRequiredFileRequest.$all(

  );
}

XmlElement _$UploadFileWithRequiredFileRequestToXml(UploadFileWithRequiredFileRequest instance) {
  final reflection = UploadFileWithRequiredFileRequestXmlReflection.instance;
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

UploadFileWithRequiredFileRequest _$UploadFileWithRequiredFileRequestFromXml(XmlElement src) {
  final reflection = UploadFileWithRequiredFileRequestXmlReflection.instance;
  return UploadFileWithRequiredFileRequest.$all(

  );
}


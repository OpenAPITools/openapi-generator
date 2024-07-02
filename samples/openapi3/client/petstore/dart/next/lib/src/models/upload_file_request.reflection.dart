// Model reflection

part of 'upload_file_request.dart';


//class reflection

class UploadFileRequestReflection extends ClassReflection<UploadFileRequest> {
  static const instance = UploadFileRequestReflection._(
    additionalMetadata: PropertyReflection(
      dartName: r'additionalMetadata',
      nullable: false,
      required: false,
      oasName: r'additionalMetadata',
      oasType: r'string',
      pattern: null,
    ),
    file: PropertyReflection(
      dartName: r'file',
      nullable: false,
      required: false,
      oasName: r'file',
      oasType: r'string',
      pattern: null,
    ),
  );
  const UploadFileRequestReflection._({
    required this.additionalMetadata,
  
    required this.file,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> additionalMetadata;
  final PropertyReflection<UndefinedWrapper<
            XFile
>> file;

  @override
  List<PropertyReflection> get members => [
    additionalMetadata,
file,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => UploadFileRequest.canDeserialize(src);
  @override
  UploadFileRequest Function(Object? src) get deserializeFunction =>
      (src) => UploadFileRequest.deserialize(src);

  @override
  Object? Function(UploadFileRequest src) get serializeFunction =>
      (src) => src.serialize();
}

class UploadFileRequestXmlReflection {
    const UploadFileRequestXmlReflection();
}


// Model reflection

part of 'upload_file_with_required_file_request.dart';


//class reflection

class UploadFileWithRequiredFileRequestReflection extends ClassReflection<UploadFileWithRequiredFileRequest> {
  static const instance = UploadFileWithRequiredFileRequestReflection._(
    additionalMetadata: PropertyReflection(
      dartName: r'additionalMetadata',
      nullable: false,
      required: false,
      oasName: r'additionalMetadata',
      oasType: r'string',
      pattern: null,
    ),
    requiredFile: PropertyReflection(
      dartName: r'requiredFile',
      nullable: false,
      required: true,
      oasName: r'requiredFile',
      oasType: r'string',
      pattern: null,
    ),
  );
  const UploadFileWithRequiredFileRequestReflection._({
    required this.additionalMetadata,
  
    required this.requiredFile,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> additionalMetadata;
  final PropertyReflection<
            XFile
> requiredFile;

  @override
  List<PropertyReflection> get members => [
    additionalMetadata,
requiredFile,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => UploadFileWithRequiredFileRequest.canDeserialize(src);
  @override
  UploadFileWithRequiredFileRequest Function(Object? src) get deserializeFunction =>
      (src) => UploadFileWithRequiredFileRequest.deserialize(src);

  @override
  Object? Function(UploadFileWithRequiredFileRequest src) get serializeFunction =>
      (src) => src.serialize();
}

class UploadFileWithRequiredFileRequestXmlReflection {
    const UploadFileWithRequiredFileRequestXmlReflection();
}


import 'package:jaguar_serializer/jaguar_serializer.dart';


part 'upload_file_body.jser.dart';

class UploadFileBody {
   /* Additional data to pass to server */
  @Alias('additionalMetadata', isNullable: false,  )
  final String additionalMetadata;
   /* file to upload */
  @Alias('file', isNullable: false,  )
  final List<int> file;
  

  UploadFileBody(
      

{
     this.additionalMetadata = null,  
     this.file = null 
    
    }
  );

  @override
  String toString() {
    return 'UploadFileBody[additionalMetadata=$additionalMetadata, file=$file, ]';
  }
}

@GenSerializer(nullableFields: true)
class UploadFileBodySerializer extends Serializer<UploadFileBody> with _$UploadFileBodySerializer {

}


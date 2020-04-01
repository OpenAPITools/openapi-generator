import 'package:jaguar_serializer/jaguar_serializer.dart';


part 'api_response.jser.dart';

class ApiResponse {
  
  @Alias('code', isNullable: false,  )
  final int code;
  
  @Alias('type', isNullable: false,  )
  final String type;
  
  @Alias('message', isNullable: false,  )
  final String message;
  

  ApiResponse(
      

{
     this.code = null,  
     this.type = null,  
     this.message = null 
    
    }
  );

  @override
  String toString() {
    return 'ApiResponse[code=$code, type=$type, message=$message, ]';
  }
}

@GenSerializer(nullableFields: true)
class ApiResponseSerializer extends Serializer<ApiResponse> with _$ApiResponseSerializer {

}


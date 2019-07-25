import 'package:jaguar_serializer/jaguar_serializer.dart';


part 'api_response.jser.dart';

class ApiResponse {
  
  @Alias('code')
  final int code;
  
  @Alias('type')
  final String type;
  
  @Alias('message')
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


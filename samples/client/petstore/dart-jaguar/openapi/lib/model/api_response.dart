import 'package:jaguar_serializer/jaguar_serializer.dart';

part 'api_response.jser.dart';

class ApiResponse {
  
  final int code;
  
  final String type;
  
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

@GenSerializer()
class ApiResponseSerializer extends Serializer<ApiResponse> with _$ApiResponseSerializer {

}

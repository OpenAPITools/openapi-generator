part of api;


@Entity()
class ApiResponse {
  
  int code = null;
  

  String type = null;
  

  String message = null;
  
  ApiResponse();

  @override
  String toString()  {
    return 'ApiResponse[code=$code, type=$type, message=$message, ]';
  }

}


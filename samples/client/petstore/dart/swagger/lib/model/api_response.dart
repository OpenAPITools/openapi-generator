part of swagger.api;

@Entity()
class ApiResponse {
  
  @Property(name: 'code')
  int code = null;
  

  @Property(name: 'type')
  String type = null;
  

  @Property(name: 'message')
  String message = null;
  
  ApiResponse();

  @override
  String toString()  {
    return 'ApiResponse[code=$code, type=$type, message=$message, ]';
  }
}


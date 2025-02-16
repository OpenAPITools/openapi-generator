import 'package:openapi_infrastructure/openapi_infrastructure.dart';

import 'apis/_exports.dart';

/// A facade class as an entry point to the generated APIs
class PetstoreApi {
  static final defaultBaseUrl = Uri.parse(r'http://petstore.swagger.io:80/v2');

  final NetworkingClientBase networkingClient;
  final Uri baseUrl;

  PetstoreApi({
    required this.networkingClient,
    Uri? baseUrl,
  }) : baseUrl = defaultBaseUrl;


  
  late final anotherFakeApi = AnotherFakeApi(
    networkingClient: networkingClient,
    baseUrl: baseUrl,
  );
  
  late final defaultApi = DefaultApi(
    networkingClient: networkingClient,
    baseUrl: baseUrl,
  );
  
  late final fakeApi = FakeApi(
    networkingClient: networkingClient,
    baseUrl: baseUrl,
  );
  
  late final fakeClassnameTags123Api = FakeClassnameTags123Api(
    networkingClient: networkingClient,
    baseUrl: baseUrl,
  );
  
  late final petApi = PetApi(
    networkingClient: networkingClient,
    baseUrl: baseUrl,
  );
  
  late final storeApi = StoreApi(
    networkingClient: networkingClient,
    baseUrl: baseUrl,
  );
  
  late final userApi = UserApi(
    networkingClient: networkingClient,
    baseUrl: baseUrl,
  );
  
  late final valuesApi = ValuesApi(
    networkingClient: networkingClient,
    baseUrl: baseUrl,
  );
  
}
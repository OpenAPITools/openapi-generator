import 'package:shared_infrastructure/shared_infrastructure.dart';

import 'apis/_exports.dart';

/// A facade class as an entry point to the generated APIs
class OpenApiPetStore {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  
  OpenApiPetStore({
    required this.networkingClient,
    required this.baseUrl,
  });

  late final PetsApi petsApi = PetsApi(
    networkingClient: networkingClient,
    baseUrl: baseUrl,
  );
}

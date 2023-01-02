//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'auth.dart';

class OAuth implements Authentication {
  OAuth({this.accessToken = ''});

  String accessToken;

  @override
  Future<void> applyToParams(
    List<QueryParam> queryParams,
    Map<String, String> headerParams,
  ) async {
    if (accessToken.isNotEmpty) {
      headerParams['Authorization'] = 'Bearer $accessToken';
    }
  }
}

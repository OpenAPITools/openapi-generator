import 'dart:convert';
import 'package:meta/meta.dart';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';
import 'dart:typed_data';

part 'values_api.models.dart';

class ValuesApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> context;

  const ValuesApi({
    required this.networkingClient,
    required this.baseUrl,
    this.context = const {},
  });

  Future<ValuesApiGetSomeValuesResponse> getSomeValues(
    ValuesApiGetSomeValuesRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return ValuesApiGetSomeValuesResponse.fromResponse(
      response,
      context: newContext,
    );
  }
}

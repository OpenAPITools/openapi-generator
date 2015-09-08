part of api;

abstract class Authentication {
	
	/// Apply authentication settings to header and query params.
	void applyToParams(Map<String, String> queryParams, Map<String, String> headerParams);
}
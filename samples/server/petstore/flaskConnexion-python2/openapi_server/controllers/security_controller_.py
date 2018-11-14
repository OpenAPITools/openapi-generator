
def info_from_api_key_api_key(api_key, required_scopes):
    """
    Returned value will be available in request['token_info'].
    'sub' or 'uid' will be set in request['user'].
    Should return None if api_key is invalid
    """
    return {'scopes': ['read:pets', 'write:pets'], 'uid': 'user_id'}


def info_from_token_petstore_auth(token):
    """
    Validate and decode token.
    Returned value will be available in request['token_info'].
    'sub' or 'uid' will be set in request['user'].
    'scope' or 'scopes' will be passed to scope validation function.
    Should return None if token is invalid
    """
    return {'scopes': ['read:pets', 'write:pets'], 'uid': 'user_id'}


def validate_scope_petstore_auth(required_scopes, token_scopes):
    """ Validate required scopes are included in token scope """
    return set(required_scopes).issubset(set(token_scopes))



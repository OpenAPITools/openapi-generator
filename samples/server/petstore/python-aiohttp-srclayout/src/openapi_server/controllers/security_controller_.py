from typing import List


def info_from_api_key(api_key: str, required_scopes: None) -> dict:
    """
    Check and retrieve authentication information from api_key.
    Returned value will be passed in 'token_info' parameter of your operation function, if there is one.
    'sub' or 'uid' will be set in 'user' parameter of your operation function, if there is one.
    Should return None if api_key is invalid or does not allow access to called API.
    """
    return {'uid': 'user_id'}


def info_from_petstore_auth(token: str) -> dict:
    """
    Validate and decode token.
    Returned value will be passed in 'token_info' parameter of your operation function, if there is one.
    'sub' or 'uid' will be set in 'user' parameter of your operation function, if there is one.
    'scope' or 'scopes' will be passed to scope validation function.
    Should return None if token is invalid or does not allow access to called API.
    """
    return {'scopes': ['read:pets', 'write:pets'], 'uid': 'user_id'}


def validate_scope_petstore_auth(required_scopes: List[str], token_scopes: List[str]) -> bool:
    """ Validate required scopes are included in token scope """
    return set(required_scopes).issubset(set(token_scopes))


from contextlib import AbstractAsyncContextManager
from typing import Dict

import uplink

from openapi_client.api import (
    PetApi,
    StoreApi,
    UserApi,
)


class Client(AbstractAsyncContextManager):
    pet: PetApi
    store: StoreApi
    user: UserApi

    def __init__(self, base_url: str, client: uplink.AiohttpClient, *, extra_service_params: Dict = {}):
        self.base_url = base_url
        service_params = {
            **extra_service_params,
            "base_url": base_url,
            "client": client
        }

        self.pet = PetApi(**service_params)
        self.store = StoreApi(**service_params)
        self.user = UserApi(**service_params)

        self.client = client

    async def __aexit__(self, exc_type, exc_value, traceback):
        await self.client.close()

    async def close(self):
        await self.client.close()

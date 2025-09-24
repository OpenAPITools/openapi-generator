import asyncio
from petstore_api import StoreApi, ApiClient, Configuration


async def main():
    api = StoreApi(
        api_client=ApiClient(
            configuration=Configuration(host="https://petstore.swagger.io/v2")
        )
    )

    result = await api.get_inventory()
    print(result)

    result = await api.get_inventory()
    print(result)

    result = await api.get_inventory()
    print(result)


if __name__ == "__main__":
    asyncio.run(main())

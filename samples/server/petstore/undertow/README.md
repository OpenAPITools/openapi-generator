# Swagger Undertow Server

## Start server

Run with

```
mvn package exec:exec
``

## Test

By default, all endpoints are protected by OAuth jwt token verifier. It can be turned off with config change through for development.


In order to access the server, there is a long lived token below issued by my
oauth2 server [undertow-server-oauth2](https://github.com/networknt/undertow-server-oauth2)

```
Bearer eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJ1cm46Y29tOm5ldHdvcmtudDpvYXV0aDI6djEiLCJhdWQiOiJ1cm46Y29tLm5ldHdvcmtudCIsImV4cCI6MTc4ODEzMjczNSwianRpIjoiNWtyM2ZWOHJaelBZNEJrSnNYZzFpQSIsImlhdCI6MTQ3Mjc3MjczNSwibmJmIjoxNDcyNzcyNjE1LCJ2ZXJzaW9uIjoiMS4wIiwidXNlcl9pZCI6InN0ZXZlIiwidXNlcl90eXBlIjoiRU1QTE9ZRUUiLCJjbGllbnRfaWQiOiJkZGNhZjBiYS0xMTMxLTIyMzItMzMxMy1kNmYyNzUzZjI1ZGMiLCJzY29wZSI6WyJhcGkuciIsImFwaS53Il19.gteJiy1uao8HLeWRljpZxHWUgQfofwmnFP-zv3EPUyXjyCOy3xclnfeTnTE39j8PgBwdFASPcDLLk1YfZJbsU6pLlmYXLtdpHDBsVmIRuch6LFPCVQ3JdqSQVci59OhSK0bBThGWqCD3UzDI_OnX4IVCAahcT9Bu94m5u_H_JNmwDf1XaP3Lt4I34buYMuRD9stchsnZi-tuIRkL13FARm1XA9aPZUMUXFdedBWDXo1zMREQ_qCJXOpaZDJM9Im0rIkq9wTEVU00pbRp_Vcdya3dfkFteBMHiwFVt6VNQaco5BXURDAIzXidwQxNEbX1ek03wra8AIani65ZK7fy_w
```

Postman is the best tool to test REST APIs

Add "Authorization" header with value as above token and a dummy message will return from the generated stub.




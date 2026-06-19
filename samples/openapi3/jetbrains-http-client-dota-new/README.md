# OpenDota API - Jetbrains API Client

## General API description

# Introduction The OpenDota API provides Dota 2 related data including advanced match data extracted from match replays.  You can find data that can be used to convert hero and ability IDs and other information provided by the API from the [dotaconstants](https://github.com/odota/dotaconstants) repository.  **Beginning 2018-04-22, the OpenDota API is limited to 50,000 free calls per month and 60 requests/minute** We offer a Premium Tier with unlimited API calls and higher rate limits. Check out the [API page](https://www.opendota.com/api-keys) to learn more. 

* API basepath : [https://api.opendota.com/api](https://api.opendota.com/api)
* Version : 20.0.0

## Documentation for API Endpoints

All URIs are relative to *https://api.opendota.com/api*, but will link to the `.http` file that contains the endpoint definition.
There may be multiple requests for a single endpoint, one for each example described in the OpenAPI specification.

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*BenchmarksApi* | [**benchmarksGet**](Apis/BenchmarksApi.http#benchmarksget) | **GET** /benchmarks | GET /benchmarks
*ConstantsApi* | [**constantsGet**](Apis/ConstantsApi.http#constantsget) | **GET** /constants | GET /constants
*ConstantsApi* | [**constantsResourceGet**](Apis/ConstantsApi.http#constantsresourceget) | **GET** /constants/{resource} | GET /constants
*DistributionsApi* | [**distributionsGet**](Apis/DistributionsApi.http#distributionsget) | **GET** /distributions | GET /distributions
*ExplorerApi* | [**explorerGet**](Apis/ExplorerApi.http#explorerget) | **GET** /explorer | GET /explorer
*FindMatchesApi* | [**findMatchesGet**](Apis/FindMatchesApi.http#findmatchesget) | **GET** /findMatches | GET /
*HealthApi* | [**healthGet**](Apis/HealthApi.http#healthget) | **GET** /health | GET /health
*HeroStatsApi* | [**heroStatsGet**](Apis/HeroStatsApi.http#herostatsget) | **GET** /heroStats | GET /heroStats
*HeroesApi* | [**heroesGet**](Apis/HeroesApi.http#heroesget) | **GET** /heroes | GET /heroes
*HeroesApi* | [**heroesHeroIdDurationsGet**](Apis/HeroesApi.http#heroesheroiddurationsget) | **GET** /heroes/{hero_id}/durations | GET /heroes/{hero_id}/durations
*HeroesApi* | [**heroesHeroIdItemPopularityGet**](Apis/HeroesApi.http#heroesheroiditempopularityget) | **GET** /heroes/{hero_id}/itemPopularity | GET /heroes/{hero_id}/itemPopularity
*HeroesApi* | [**heroesHeroIdMatchesGet**](Apis/HeroesApi.http#heroesheroidmatchesget) | **GET** /heroes/{hero_id}/matches | GET /heroes/{hero_id}/matches
*HeroesApi* | [**heroesHeroIdMatchupsGet**](Apis/HeroesApi.http#heroesheroidmatchupsget) | **GET** /heroes/{hero_id}/matchups | GET /heroes/{hero_id}/matchups
*HeroesApi* | [**heroesHeroIdPlayersGet**](Apis/HeroesApi.http#heroesheroidplayersget) | **GET** /heroes/{hero_id}/players | GET /heroes/{hero_id}/players
*LeaguesApi* | [**leaguesGet**](Apis/LeaguesApi.http#leaguesget) | **GET** /leagues | GET /leagues
*LeaguesApi* | [**leaguesLeagueIdGet**](Apis/LeaguesApi.http#leaguesleagueidget) | **GET** /leagues/{league_id} | GET /leagues/{league_id}
*LeaguesApi* | [**leaguesLeagueIdMatchesGet**](Apis/LeaguesApi.http#leaguesleagueidmatchesget) | **GET** /leagues/{league_id}/matches | GET /leagues/{league_id}/matches
*LeaguesApi* | [**leaguesLeagueIdTeamsGet**](Apis/LeaguesApi.http#leaguesleagueidteamsget) | **GET** /leagues/{league_id}/teams | GET /leagues/{league_id}/teams
*LiveApi* | [**liveGet**](Apis/LiveApi.http#liveget) | **GET** /live | GET /live
*MatchesApi* | [**matchesMatchIdGet**](Apis/MatchesApi.http#matchesmatchidget) | **GET** /matches/{match_id} | GET /matches/{match_id}
*MetadataApi* | [**metadataGet**](Apis/MetadataApi.http#metadataget) | **GET** /metadata | GET /metadata
*ParsedMatchesApi* | [**parsedMatchesGet**](Apis/ParsedMatchesApi.http#parsedmatchesget) | **GET** /parsedMatches | GET /parsedMatches
*PlayersApi* | [**playersAccountIdCountsGet**](Apis/PlayersApi.http#playersaccountidcountsget) | **GET** /players/{account_id}/counts | GET /players/{account_id}/counts
*PlayersApi* | [**playersAccountIdGet**](Apis/PlayersApi.http#playersaccountidget) | **GET** /players/{account_id} | GET /players/{account_id}
*PlayersApi* | [**playersAccountIdHeroesGet**](Apis/PlayersApi.http#playersaccountidheroesget) | **GET** /players/{account_id}/heroes | GET /players/{account_id}/heroes
*PlayersApi* | [**playersAccountIdHistogramsFieldGet**](Apis/PlayersApi.http#playersaccountidhistogramsfieldget) | **GET** /players/{account_id}/histograms/{field} | GET /players/{account_id}/histograms
*PlayersApi* | [**playersAccountIdMatchesGet**](Apis/PlayersApi.http#playersaccountidmatchesget) | **GET** /players/{account_id}/matches | GET /players/{account_id}/matches
*PlayersApi* | [**playersAccountIdPeersGet**](Apis/PlayersApi.http#playersaccountidpeersget) | **GET** /players/{account_id}/peers | GET /players/{account_id}/peers
*PlayersApi* | [**playersAccountIdProsGet**](Apis/PlayersApi.http#playersaccountidprosget) | **GET** /players/{account_id}/pros | GET /players/{account_id}/pros
*PlayersApi* | [**playersAccountIdRankingsGet**](Apis/PlayersApi.http#playersaccountidrankingsget) | **GET** /players/{account_id}/rankings | GET /players/{account_id}/rankings
*PlayersApi* | [**playersAccountIdRatingsGet**](Apis/PlayersApi.http#playersaccountidratingsget) | **GET** /players/{account_id}/ratings | GET /players/{account_id}/ratings
*PlayersApi* | [**playersAccountIdRecentMatchesGet**](Apis/PlayersApi.http#playersaccountidrecentmatchesget) | **GET** /players/{account_id}/recentMatches | GET /players/{account_id}/recentMatches
*PlayersApi* | [**playersAccountIdRefreshPost**](Apis/PlayersApi.http#playersaccountidrefreshpost) | **POST** /players/{account_id}/refresh | POST /players/{account_id}/refresh
*PlayersApi* | [**playersAccountIdTotalsGet**](Apis/PlayersApi.http#playersaccountidtotalsget) | **GET** /players/{account_id}/totals | GET /players/{account_id}/totals
*PlayersApi* | [**playersAccountIdWardmapGet**](Apis/PlayersApi.http#playersaccountidwardmapget) | **GET** /players/{account_id}/wardmap | GET /players/{account_id}/wardmap
*PlayersApi* | [**playersAccountIdWlGet**](Apis/PlayersApi.http#playersaccountidwlget) | **GET** /players/{account_id}/wl | GET /players/{account_id}/wl
*PlayersApi* | [**playersAccountIdWordcloudGet**](Apis/PlayersApi.http#playersaccountidwordcloudget) | **GET** /players/{account_id}/wordcloud | GET /players/{account_id}/wordcloud
*PlayersByRankApi* | [**playersByRankGet**](Apis/PlayersByRankApi.http#playersbyrankget) | **GET** /playersByRank | GET /playersByRank
*ProMatchesApi* | [**proMatchesGet**](Apis/ProMatchesApi.http#promatchesget) | **GET** /proMatches | GET /proMatches
*ProPlayersApi* | [**proPlayersGet**](Apis/ProPlayersApi.http#proplayersget) | **GET** /proPlayers | GET /proPlayers
*PublicMatchesApi* | [**publicMatchesGet**](Apis/PublicMatchesApi.http#publicmatchesget) | **GET** /publicMatches | GET /publicMatches
*RankingsApi* | [**rankingsGet**](Apis/RankingsApi.http#rankingsget) | **GET** /rankings | GET /rankings
*RecordsApi* | [**recordsFieldGet**](Apis/RecordsApi.http#recordsfieldget) | **GET** /records/{field} | GET /records/{field}
*ReplaysApi* | [**replaysGet**](Apis/ReplaysApi.http#replaysget) | **GET** /replays | GET /replays
*RequestApi* | [**requestJobIdGet**](Apis/RequestApi.http#requestjobidget) | **GET** /request/{jobId} | GET /request/{jobId}
*RequestApi* | [**requestMatchIdPost**](Apis/RequestApi.http#requestmatchidpost) | **POST** /request/{match_id} | POST /request/{match_id}
*ScenariosApi* | [**scenariosItemTimingsGet**](Apis/ScenariosApi.http#scenariositemtimingsget) | **GET** /scenarios/itemTimings | GET /scenarios/itemTimings
*ScenariosApi* | [**scenariosLaneRolesGet**](Apis/ScenariosApi.http#scenarioslanerolesget) | **GET** /scenarios/laneRoles | GET /scenarios/laneRoles
*ScenariosApi* | [**scenariosMiscGet**](Apis/ScenariosApi.http#scenariosmiscget) | **GET** /scenarios/misc | GET /scenarios/misc
*SchemaApi* | [**schemaGet**](Apis/SchemaApi.http#schemaget) | **GET** /schema | GET /schema
*SearchApi* | [**searchGet**](Apis/SearchApi.http#searchget) | **GET** /search | GET /search
*StatusApi* | [**statusGet**](Apis/StatusApi.http#statusget) | **GET** /status | GET /status
*TeamsApi* | [**teamsGet**](Apis/TeamsApi.http#teamsget) | **GET** /teams | GET /teams
*TeamsApi* | [**teamsTeamIdGet**](Apis/TeamsApi.http#teamsteamidget) | **GET** /teams/{team_id} | GET /teams/{team_id}
*TeamsApi* | [**teamsTeamIdHeroesGet**](Apis/TeamsApi.http#teamsteamidheroesget) | **GET** /teams/{team_id}/heroes | GET /teams/{team_id}/heroes
*TeamsApi* | [**teamsTeamIdMatchesGet**](Apis/TeamsApi.http#teamsteamidmatchesget) | **GET** /teams/{team_id}/matches | GET /teams/{team_id}/matches
*TeamsApi* | [**teamsTeamIdPlayersGet**](Apis/TeamsApi.http#teamsteamidplayersget) | **GET** /teams/{team_id}/players | GET /teams/{team_id}/players


## Usage

### Prerequisites

You need [IntelliJ](https://www.jetbrains.com/idea/) to be able to run those queries. More information can be found [here](https://www.jetbrains.com/help/idea/http-client-in-product-code-editor.html).
You may have some luck running queries using the [Code REST Client](https://marketplace.visualstudio.com/items?itemName=humao.rest-client) as well, but your mileage may vary.

### Variables and Environment files

* Generally speaking, you want queries to be specific using custom variables. All variables in the `.http` files have the `` format.
* You can create [public or private environment files](https://www.jetbrains.com/help/idea/exploring-http-syntax.html#environment-variables) to dynamically replace the variables at runtime.

_Note: don't commit private environment files! They typically will contain sensitive information like API Keys._

### Customizations

If you have control over the generation of the files here, there are two main things you can do

* Select elements to replace as variables during generation. The process is case-sensitive. For example, API_KEY -> 
    * For this, run the generation with the `bodyVariables` property, followed by a "-" separated list of variables
    * Example: `--additional-properties bodyVariables=YOUR_MERCHANT_ACCOUNT-YOUR_COMPANY_ACCOUNT-YOUR_BALANCE_PLATFORM`
* Add custom headers to _all_ requests. This can be useful for example if your specifications are missing [security schemes](https://github.com/github/rest-api-description/issues/237).
    * For this, run the generation with the `customHeaders` property, followed by a "&" separated list of variables
    * Example : `--additional-properties=customHeaders="Cookie:X-API-KEY="&"Accept-Encoding=gzip"`

_This client was generated by the [jetbrains-http-client](https://openapi-generator.tech/docs/generators/jetbrains-http-client) generator of OpenAPI Generator_
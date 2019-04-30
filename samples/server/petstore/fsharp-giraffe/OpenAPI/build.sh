#!/bin/sh
dotnet restore src/OpenAPI.fsproj
dotnet build --project src/OpenAPI.fsproj


#See https://aka.ms/containerfastmode to understand how Visual Studio uses this Dockerfile to build your images for faster debugging.

# Container we use for final publish
FROM mcr.microsoft.com/dotnet/core/aspnet:3.0-buster-slim AS base
WORKDIR /app
EXPOSE 80
EXPOSE 443

# Build container
FROM mcr.microsoft.com/dotnet/core/sdk:3.0-buster AS build

# Copy the code into the container
WORKDIR /src
COPY ["src/Org.OpenAPITools/Org.OpenAPITools.csproj", "Org.OpenAPITools/"]

# NuGet restore
RUN dotnet restore "Org.OpenAPITools/Org.OpenAPITools.csproj"
COPY ["src/Org.OpenAPITools/", "Org.OpenAPITools/"]

# Build the API
WORKDIR "Org.OpenAPITools"
RUN dotnet build "Org.OpenAPITools.csproj" -c Release -o /app/build

# Publish it
FROM build AS publish
RUN dotnet publish "Org.OpenAPITools.csproj" -c Release -o /app/publish

# Make the final image for publishing
FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Org.OpenAPITools.dll"]

FROM microsoft/dotnet:latest

RUN groupadd -r app && useradd -r -g app app

ENV DOTNET_CLI_TELEMETRY_OPTOUT 1

RUN mkdir -p /app/IO.Swagger
COPY . /app/IO.Swagger
WORKDIR /app/IO.Swagger

EXPOSE 5000/tcp

USER app

RUN ["dotnet", "restore"]
ENTRYPOINT ["dotnet", "run", "-p", "project.json", "web"]

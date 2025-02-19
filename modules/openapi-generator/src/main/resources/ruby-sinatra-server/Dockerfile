## Build libraries
FROM ruby:3.0-alpine as rubydev

## for thin or falcon
#RUN apk --no-cache add make g++ libc-dev
## for puma
#RUN apk --no-cache add make gcc libc-dev

ADD . /app
WORKDIR /app

RUN bundle config set path lib
RUN bundle install

## Build Runtime image
FROM ruby:3.0-alpine

RUN apk --no-cache add tzdata ## ca-certificates

COPY --from=rubydev /app /app
WORKDIR /app

ENV SINATRA_HOST 0.0.0.0
ENV SINATRA_PORT 8080
EXPOSE $SINATRA_PORT

RUN addgroup sinatra
RUN adduser -S -G sinatra sinatra
USER sinatra
RUN bundle config set path lib

CMD bundle exec rackup --host $SINATRA_HOST -p $SINATRA_PORT

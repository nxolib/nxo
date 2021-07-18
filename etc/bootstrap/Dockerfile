## build with:
#
# docker build -t nxo-bs .

FROM node:10-stretch

RUN apt-get update \
  && apt-get install ruby-dev rubygems --assume-yes \
  && gem update --system \
  && gem install sass --no-user-install

WORKDIR /css
ENTRYPOINT ["sass", "-t", "compressed", \
  "etc/bootstrap/custom.scss", "priv/static/css/bootstrap.css"]

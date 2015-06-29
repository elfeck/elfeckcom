FROM haskell:7.8.4

RUN cabal update && mkdir /var/elfeckcom && mkdir /var/elfeckcom/private && mkdir /var/elfeckcom/public


# Install Dep

ADD ./elfeckcom.cabal /var/elfeckcom/private/elfeckcom.cabal
ADD ./LICENSE /var/elfeckcom/private/LICENSE
RUN cd /var/elfeckcom/private && cabal install --dependencies-only


# Install App

ADD ./src /var/elfeckcom/private/src
RUN cd /var/elfeckcom/private && cabal install


# Copy static files

ADD ./static /var/elfeckcom/public/static


# Run

WORKDIR /var/elfeckcom/public
CMD ["elfeckcom", "/var/elfeckcom/private/config.txt"]

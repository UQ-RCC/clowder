# ----------------------------------------------------------------------
# BUILD CLOWDER DIST
# ----------------------------------------------------------------------
# FROM java:jdk-alpine as clowder-build
FROM openjdk:8-jdk as clowder-build

ARG BRANCH
ARG VERSION
ARG BUILDNUMBER
ARG GITSHA1

WORKDIR /src

# install clowder libraries (hopefully cached)
COPY sbt* /src/
COPY project /src/project
RUN ./sbt update

# environemnt variables
ENV BRANCH=${BRANCH} \
    VERSION=${VERSION} \
    BUILDNUMBER=${BUILDNUMBER} \
    GITSHA1=${GITSHA1}

# compile clowder
COPY lib /src/lib/
COPY conf /src/conf/
COPY public /src/public/
COPY app /src/app/
RUN rm -rf target/universal/clowder-*.zip clowder clowder-* \
    && ./sbt dist \
    && unzip -q target/universal/clowder-*.zip \
    && mv clowder-* clowder \
    && apk add --no-cache zip \
    && for x in $(find clowder -name \*.jar); do \
         zip -d $x org/apache/log4j/net/JMSAppender.class org/apache/log4j/net/SocketServer.class | grep 'deleting:' && echo "fixed $x"; \
       done; \
       echo "removed JMSAppender and SocketServer" \
    && mkdir -p clowder/custom clowder/logs

# ----------------------------------------------------------------------
# BUILD CLOWDER
# ----------------------------------------------------------------------
#FROM java:jre-alpine
FROM openjdk:8-jdk

# add bash
RUN apt-get update 
RUN apt-get install -y bash curl bind9
#RUN apk add --no-cache bash curl

# environemnt variables
ARG BRANCH
ARG VERSION
ARG BUILDNUMBER
ARG GITSHA1
ENV BRANCH=${BRANCH} \
    VERSION=${VERSION} \
    BUILDNUMBER=${BUILDNUMBER} \
    GITSHA1=${GITSHA1}

# expose some properties of the container
EXPOSE 9000

# working directory
WORKDIR /home/clowder

# customization including data
VOLUME /home/clowder/custom /home/clowder/data

# copy the build file, this requires sbt dist to be run (will be owned by root)
COPY --chown=0:0 --from=clowder-build /src/clowder /home/clowder/
COPY docker/clowder.sh docker/healthcheck.sh /home/clowder/
COPY docker/custom.conf docker/play.plugins /home/clowder/custom/

# add letsecrypt to 
# download https://letsencrypt.org/certs/letsencryptauthorityx1.pem
RUN wget https://letsencrypt.org/certs/letsencryptauthorityx1.pem -O /tmp/letsencryptauthorityx1.pem
RUN keytool -import -alias letsecrypt -file /tmp/letsencryptauthorityx1.pem -keystore ${JAVA_HOME}/jre/lib/security/cacerts -storepass changeit

# Containers should NOT run as root as a good practice
# numeric id to be compatible with openshift, will run as random userid:0
RUN mkdir -p /home/clowder/data && \
    chmod 777 /home/clowder/logs /home/clowder/data /home/clowder/custom
USER 10001

# command to run when starting docker
CMD /home/clowder/clowder.sh

# health check
HEALTHCHECK CMD /home/clowder/healthcheck.sh

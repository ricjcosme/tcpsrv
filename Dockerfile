FROM		alpine:latest

MAINTAINER	Ricardo Cosme <ricjcosme@gmail.com>

RUN    		apk add --update curl tar xz bash git && \
       		echo "http://dl-4.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories && \
       		echo "http://dl-4.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories && \
       		echo "http://dl-4.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories && \
       		apk add erlang erlang erlang-mnesia erlang-public-key erlang-crypto erlang-ssl \
       			erlang-sasl erlang-asn1 erlang-inets erlang-os-mon erlang-xmerl erlang-eldap \
               		erlang-syntax-tools --update-cache --allow-untrusted && \
       		apk del --purge tar xz && rm -Rf /var/cache/apk/*

RUN    		git clone https://github.com/ricjcosme/tcpsrv.git /srv/tcpsrv && cd /srv/tcpsrv \
       		./rebar3 compile && ./rebar3 release

WORKDIR		/srv/tcpsrv

VOLUME		["/srv/tcpsrv"]

EXPOSE 		10001/tcp

ENTRYPOINT	["/srv/tcpsrv/_build/default/rel/tcpsrv/bin/tcpsrv"]

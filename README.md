tcpsrv
======

A small and simple TCP server and dispatcher


This TCP server and dispatcher was initially implemented for running in embedded systems but I've broaden its scope and now it also runs smoothly on medium / high level throughput systems. Its main purpose is to enable transfering small quantities of data from as many remote sources as needed and route that data into local datastores or elsewhere by spawning each connection, incoming data and delivery into a new Erlang process.

Ideal for big data solutions requiring many small data flows like remote sensing, telemetry readings, raw byte comms, logging, etc.

The number of concurrent listening processes on its port can be changed by setting the OS env variable TCPSRV_SERVERS (defaults to 10)


Build (manually)
-----

```Bash
	$ git clone https://github.com/ricjcosme/tcpsrv.git
	$ cd tcpsrv	
	$ ./rebar3 compile
	$ ./rebar3 release
```
Before you can run it, you need to define these OS env variables (have a look at ./Dockerfile ENV definitions for examples):

- TCPSRV_PORT - the listening port
- TCPSRV_SERVERS - the number of spawned listening workers (max. concurrent connections allowed)
- TCPSRV_TIMEOUT - the amount of time the socket is idle before closing
- TCPSRV_PACKET - the type of packet for the socket [possible values here](http://erlang.org/doc/man/inet.html#setopts-2)
- TCPSRV\_BINARY\_OR_LIST - define how packets are to be received (possible values are binary or list)
- TCPSRV_DATASTORE - the destination datastore to use (elasticsearch, solr or redis currently supported - more cand be added to function toDataStore/2 in ./src/srv.erl)
- TCPSRV\_DST_HOST - hostname or ip of destination datastore machine
- TCPSRV\_EL\_HOST_PORT - hostname:port combination for elasticsearch datastore
- TCPSRV\_EL_INDEX - index name for elasticsearch datastore
- TCPSRV\_EL_MAPPING - mapping name for index at elasticsearch datastore
- TCPSRV\_SOLR\_HOST_PORT - hostname:port combination for solr datastore
- TCPSRV\_SOLR_CORE - solr collection / core name
- TCPSRV\_REDIS_PORT - redis datastore port

Now you start it by running: 

```Bash
	$ ./_build/default/rel/tcpsrv/bin/tcpsrv
```

Build (Docker)
-----

There's also a Dockerfile definition so you can run it as a micro-service. If you want to make changes to the OS env variables that are used as default values, [download the Dockerfile](https://raw.githubusercontent.com/ricjcosme/tcpsrv/master/Dockerfile) and change them at the ENV section. You can then build the Dockerfile by running the following command (Dockerfile must be in the same dir):

```Bash
	$ sudo docker build -t tcpsrv .
```

Otherwise, just build directly using this git repo:

```Bash
	$ sudo docker build -t tcpsrv https://github.com/ricjcosme/tcpsrv.git
```

Now you can launch a container with tcpsrv by running:

```Bash
	$ sudo docker run -i -t --name tcpsrv -d -p 10001:10001 --restart=always tcpsrv
```




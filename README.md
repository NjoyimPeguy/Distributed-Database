# Distributed-Database
A minimal distributed database done in Erlang

Usage
-----

Server : "./server.sh function nodename"  
Client : "./client.sh function server-name" 

###### Example:
  
Starting a new network : `./server.sh`
  
Joining a server : `./server.sh join <existing-node>`

Client connecting to a server : `./client.sh start_connection <existing-node>`

##### Monitor:

running the monitor: ./monitor.sh

###### Monitor commands:
A new database network with one server whose name is `main` : `Command> create main`

Connection to a server/node : `Command> connect main@localhost`

A server with a node name `server30` wants to join a network :`Command> join server30`

Printing the status of a given network from a given a server : `Command> status main@localhost`

Stopping a given server : `Command> stop server30@localhost`

##### Query:

###### Example:

`Query> write hello`

`Query> The client has just received the key : 80WmDv9aSgaOILGEolrm4A`

`Query> read 80WmDv9aSgaOILGEolrm4A`

`Query> The client is reading the entry {80WmDv9aSgaOILGEolrm4A, hello}`




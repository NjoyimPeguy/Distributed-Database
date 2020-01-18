# Distributed-Database
A minimal distributed database done in Erlang

Usage
-----

Running bash script
-------------------

Server example : "./server.sh \<function\> \<node-name\>"  
Client example : "./client.sh \<function\> \<server-name\>"  
  
Starting a new network : `./server.sh`
  
Joining a server : `./server.sh join <existing-node>`

Client connecting to a server : `./client.sh start_connection <existing-node>`

##### Query:

###### Example:

`Query> write hello`

`Query> The client has just received the key : 80WmDv9aSgaOILGEolrm4A`

`Query> read 80WmDv9aSgaOILGEolrm4A`

`Query> The client is reading the entry {80WmDv9aSgaOILGEolrm4A, hello}`



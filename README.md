# Distributed-Database
A minimal distributed database done in Erlang

#### Running bash script
Server example : "./server.sh \<function\> \<node-name\>"  
Client example : "./client.sh \<function\> \<server-name\>"  
  

Starting the first node/server : `./server.sh start <node>`  
  
Joining a server : `./server.sh join <existing-node>`

Client connecting to a server : `./client.sh start_connection <existing-node>`


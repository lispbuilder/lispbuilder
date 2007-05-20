#!/bin/sh

g++ -g2 server.cpp net-linux.cpp net-common.cpp -o server
g++ -g2 client.cpp net-linux.cpp net-common.cpp -o client

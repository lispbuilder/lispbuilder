#!/bin/sh

g++ -g2 server.cpp net-mac.cpp net-common.cpp -o server
g++ -g2 client.cpp net-mac.cpp net-common.cpp -o client

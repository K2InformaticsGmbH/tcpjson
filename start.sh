#!/bin/bash

# Parameters: Port [SSL] [Interface]

port=$1
ssl=$2
interface=$3
if [ "$#" -ne 1 ]; then
    port=7443
fi
if [ "$#" -ne 2 ]; then
    ssl="true"
fi
if [ "$#" -ne 3 ]; then
    interface="{127,0,0,1}"
fi

if [[ "$unamestr" == 'Linux' || "$unamestr" == 'Darwin' ]]; then
     exename=erl
else
    exename='start //MAX werl.exe'
    #exename='erl.exe'
fi

# PATHS
# PATHS
paths="-pa"
paths=$paths" ebin"
paths=$paths" deps/*/ebin"

# dderl opts
opts="-etcpjson"
opts=$opts" port $port"
opts=$opts" ssl $ssl"
opts=$opts" ssl_opts [{certfile,\"priv/certs/server.crt\"},{keyfile,\"priv/certs/server.key\"}]"
opts=$opts" interface $interface"

# sasl opts
sasl_opts="-sasl"
sasl_opts=$sasl_opts"  sasl_error_logger false" 

start_opts="$paths $opts $sasl_opts"

# ETCPJSON start options
echo "------------------------------------------"
echo "Starting ETCPJSON (Opts)"
echo "------------------------------------------"
echo "EBIN Path : $paths"
echo "DDERL     : $opts"
echo "SASL      : $sasl_opts"
echo "------------------------------------------"

# Starting etcpjson
$exename $start_opts -s etcpjson

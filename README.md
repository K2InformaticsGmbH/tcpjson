# tcpjson : TCP/SSL server and client with JSON transport

## Current implementations
### servers
* erlang
### clients
* python

## Integration and use

1. add `etcpjson` to your project's dependency and to `included_applications` list of your erlang application in `.app` (`.app.src`)
1. implement `etcjson_srv` behavior in your application (e.g. in `myapp_etcjson` module) see `etcpjson_echo` module as an exmaple implementation
1. start your ranch listener as follows in your erlang application's code:
```erlang
ranch:start_listener(
    ref(), NumAcceptors, Transport, TransOpts,
    etcpjson_srv, [myapp_etcjson]
),
```
Ref : [ranch function reference](https://ninenines.eu/docs/en/ranch/1.3/manual/ranch/)

## Demo (Erlang Server)

The provided `start.sh` will start `etcpjson` application at 127.0.0.1:7443 with SSL (certificates from priv/certs folder). A echo handler (`etcjson_echo`) will be automatically attached to handle the TCP connections and will wrap any received JSON as follows:
```json
{
    "echo": _RECEIVED_JSON_
}
```
and echo back after 2 seconds
example server log:
```erlang
Erlang/OTP 19 [erts-8.3] [64-bit] [smp:4:4] [async-threads:10]

Eshell V8.3  (abort with ^G)
1> 11:23:09.925 [info] Application lager started on node nonode@nohost
11:23:09.925 [info] ssl server listening 127.0.0.1:7443 [{ip,{127,0,0,1}},{port,7443},{certfile,"priv/certs/server.crt"},{keyfile,"priv/certs/server.key"}]
11:23:10.298 [warning] lager_error_logger_h dropped 58 messages in the last second that exceeded the limit of 50 messages/sec
...
9> 11:36:29.849 [info] peer connected 127.0.0.1:36569
11:36:32.678 [info] RX: 1
11:36:39.583 [info] RX: 1234
11:36:52.586 [info] RX: #{<<"a">> => <<"test">>}
11:37:04.961 [info] peer closed 127.0.0.1:36569
11:37:04.962 [info] terminate 127.0.0.1:36569 : normal
12:01:34.984 [info] peer connected 127.0.0.1:37118
12:01:41.636 [info] RX: #{<<"a">> => 123}
...
12:04:20.778 [info] peer closed 127.0.0.1:37118
12:04:20.778 [info] terminate 127.0.0.1:37118 : normal
```

## Demo (Python Client)

A sample python client is provided in (clients/python.py)
usage:
```sh
python.exe clients/python/echo_client.py -h
usage: python.py [-h] [-ip IP_ADDRESS] [-p PORT] [-ns]

TCP/SSL JSON Client

optional arguments:
  -h, --help            show this help message and exit
  -ip IP_ADDRESS, --ip_address IP_ADDRESS
                        server IP address (default 127.0.0.1)
  -p PORT, --port PORT  server TCP/SSL port (default 7443)
  -ns, --no-ssl         server type TCP or SSL (default)
```

example client log:
```sh
python.exe clients/python/echo_client.py
2017-06-15 13:21:46 SSL connecting to 127.0.0.1:7443
 -> 1
2017-06-15 13:21:48 TX:    1
2017-06-15 13:21:48 TXRAW: b'\x01\x00\x00\x001'
10
2017-06-15 13:21:50 RXRAW: b'\n\x00\x00\x00{"echo":1}'
2017-06-15 13:21:50 RX: {
    "echo": 1
}
2017-06-15 13:21:50 RXJSON : {'echo': 1}
 -> 1.1
2017-06-15 13:21:51 TX:    1.1
2017-06-15 13:21:51 TXRAW: b'\x03\x00\x00\x001.1'
12
2017-06-15 13:21:53 RXRAW: b'\x0c\x00\x00\x00{"echo":1.1}'
2017-06-15 13:21:53 RX: {
    "echo": 1.1
}
2017-06-15 13:21:53 RXJSON : {'echo': 1.1}
 -> "test"
2017-06-15 13:21:56 TX:    "test"
2017-06-15 13:21:56 TXRAW: b'\x06\x00\x00\x00"test"'
15
2017-06-15 13:21:58 RXRAW: b'\x0f\x00\x00\x00{"echo":"test"}'
2017-06-15 13:21:58 RX: {
    "echo": "test"
}
2017-06-15 13:21:58 RXJSON : {'echo': 'test'}
 -> [1,2]
2017-06-15 13:22:05 TX:    [
    1,
    2
]
2017-06-15 13:22:05 TXRAW: b'\x06\x00\x00\x00[1, 2]'
14
2017-06-15 13:22:07 RXRAW: b'\x0e\x00\x00\x00{"echo":[1,2]}'
2017-06-15 13:22:07 RX: {
    "echo": [
        1,
        2
    ]
}
2017-06-15 13:22:07 RXJSON : {'echo': [1, 2]}
 -> {"test":1}
2017-06-15 13:22:33 TX:    {
    "test": 1
}
2017-06-15 13:22:33 TXRAW: b'\x0b\x00\x00\x00{"test": 1}'
19
2017-06-15 13:22:35 RXRAW: b'\x13\x00\x00\x00{"echo":{"test":1}}'
2017-06-15 13:22:35 RX: {
    "echo": {
        "test": 1
    }
}
2017-06-15 13:22:35 RXJSON : {'echo': {'test': 1}}
 -> q
```

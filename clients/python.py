import socket
import ssl
import json
import argparse
import datetime

def Main():
    parser = argparse.ArgumentParser(description='TCP/SSL JSON Client')
    parser.add_argument(
            '-ip', '--ip_address', default='127.0.0.1',
            help='server IP address (default 127.0.0.1)')
    parser.add_argument(
            '-p', '--port', default=7443, type=int,
            help='server TCP/SSL port (default 7443)')
    parser.add_argument(
            '-ns', '--no-ssl', action="store_true",
            help='server type TCP or SSL (default)')

    args = parser.parse_args()
    host = args.ip_address
    port = args.port

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    if not args.no_ssl :
        print(Ts() + "SSL connecting to " + host + ":" + str(port))
        sock = ssl.wrap_socket(sock)
    else :
        print("connecting to " + host + ":" + str(port))

    sock.connect((host,port))

    message = input(" -> ")

    while message != 'q':

        ## TX JSON with unsigned 4-bytes length header
        jsn = json.loads(message)                                       # check if valid json
        tx = json.dumps(jsn).encode('utf-8')                            # serialize to utf-8 bytes
        txlen = socket.htonl(len(tx))                                   # calculate payload length into network-byte-order
        tx = txlen.to_bytes(4, byteorder = 'big') + tx                  # prepend 32 bit length header
        print(Ts() + 'TX:    ' + json.dumps(jsn, sort_keys=True, indent=4))
        print(Ts() + 'TXRAW: ', end='')
        print(tx, flush=True)
        sock.send(tx)                                                   # send
        
        ## RX JSON with unsigned 4-bytes length header
        data = sock.recv(4)                                             # receive only 4 bytes first (32 bit length header)
        rxlen = socket.ntohl(int.from_bytes(data, byteorder='big'))     # convert host-byte-order to integer
        print(rxlen)
        rx = sock.recv(rxlen)                                           # receive a full json binary and decode to string
        jsn = json.loads(rx.decode())                                   # decode json to python object
        print(Ts() + 'RXRAW: ', end='')
        print(data + rx)
        print(Ts() + 'RX: ' + json.dumps(jsn, sort_keys=True, indent=4), flush=True)
        message = input(" -> ")
                 
    sock.close()

def Ts():
    return '{:%Y-%m-%d %H:%M:%S} '.format(datetime.datetime.now())
    
if __name__ == '__main__':
    Main()

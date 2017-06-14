import socket
import ssl
import json
import argparse
 
def Main():
    parser = argparse.ArgumentParser(description='TCP Client')
    parser.add_argument(
            '-ip', '--ip_address', default='127.0.0.1', metavar='IP', help='server IP address')
    parser.add_argument(
            '-p', '--port', default=7443, metavar='Port', type=int, help='server TCP/SSL port')
    parser.add_argument(
            '-s', '--ssl', default=False, metavar='SSL', type=str2bool, nargs='?', const=True,
            help='server type TCP or SSL')

    args = parser.parse_args()
    print(args)

    host = args.ip_address
    port = args.port

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    if args.ssl :
        sock = ssl.wrap_socket(sock)

    sock.connect((host,port))

    message = input(" -> ")

    while message != 'q':

        ## TX JSON with unsigned 4-bytes length header
        jsn = json.loads(message)                                       # check if valid json
        tx = json.dumps(jsn).encode('utf-8')                            # serialize to utf-8 bytes
        txlen = socket.htonl(len(tx))                                   # calculate payload length into network-byte-order
        tx = txlen.to_bytes(4, byteorder = 'big') + tx                  # prepend 32 bit length header
        print('TX:    ' + json.dumps(jsn, sort_keys=True, indent=4))
        print('TXRAW: ', end='')
        print(tx)
        sock.send(tx)                                                   # send
        
        ## RX JSON with unsigned 4-bytes length header
        data = sock.recv(4)                                             # receive only 4 bytes first (32 bit length header)
        rxlen = socket.ntohl(int.from_bytes(data, byteorder='big'))     # convert host-byte-order to integer
        print(rxlen)
        rx = sock.recv(rxlen)                                           # receive a full json binary and decode to string
        jsn = json.loads(rx.decode())                                   # decode json to python object
        print('RXRAW: ', end='')
        print(data + rx)
        print('RX: ' + json.dumps(jsn, sort_keys=True, indent=4))
        message = input(" -> ")
                 
    sock.close()
 
def str2bool(v):
    if v.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif v.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')

if __name__ == '__main__':
    Main()

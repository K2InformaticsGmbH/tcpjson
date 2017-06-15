import argparse
import datetime
import tcpjson 
from tcpjson import TcpJson

def Main():
    parser = argparse.ArgumentParser(description='TCP/SSL JSON Echo Client')
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

    tj = TcpJson(host, port, not args.no_ssl)
    message = input(" -> ")

    while message != 'q':

        tj.Send(message)

        Rx = tj.Recv()
        print(tcpjson.Ts() + 'RXJSON : ', end='')
        print(Rx)
        message = input(" -> ")

if __name__ == '__main__':
    Main()

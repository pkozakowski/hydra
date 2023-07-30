import os
import sys

import msgpackrpc


def load_tws(tws_path: str) -> None:
    sys.path.append(os.path.join(tws_path, 'source', 'pythonclient'))


if __name__ == '__main__':
    tws_path = sys.argv[1]
    load_tws(tws_path)

    import server

    port = int(sys.argv[2])
    server = msgpackrpc.Server(rpc.Server())
    server.listen(msgpackrpc.Address('localhost', port))
    server.start()

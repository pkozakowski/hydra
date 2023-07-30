import multiprocessing as mp

from ibapi import client, common, wrapper


class Client(wrapper.EWrapper, client.EClient):
    def __init__(self, host: str, port: int, id: int, response: mp.Queue):
        wrapper.EWrapper.__init__(self)
        client.EClient.__init__(self, self)
        super().connect(host, port, id)
        self._output = response

    def symbolSamples(self, reqId: int, descs: list) -> None:
        super().symbolSamples(reqId, descs)
        self._output.put(descs)
        self._output.put(EndOfResponse)
        self.disconnect()

    def historicalData(self, reqId: int, bar: common.BarData) -> None:
        super().historicalData(reqId, bar)
        self._output.put(bar)

    def historicalDataEnd(self, reqId: int, start: str, end: str) -> None:
        super().historicalDataEnd(reqId, start, end)
        self._output.put(EndOfResponse)
        self.disconnect()


class EndOfResponse:
    pass

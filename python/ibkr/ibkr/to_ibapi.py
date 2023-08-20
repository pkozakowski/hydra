import multiprocessing as mp
import logging

from ibapi import client, common, wrapper

ibapi_logger = logging.getLogger("ibapi.wrapper")


class Client(wrapper.EWrapper, client.EClient):
    def __init__(self, host: str, port: int, id: int, response: mp.Queue):
        wrapper.EWrapper.__init__(self)
        client.EClient.__init__(self, self)
        super().connect(host, port, id)
        self._output = response
        ibapi_logger.no_data_callbacks.append(self.no_data_callback)

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

    def no_data_callback(self):
        self._output.put(EndOfResponse)
        ibapi_logger.no_data_callbacks.pop()
        self.disconnect()


class EndOfResponse:
    pass

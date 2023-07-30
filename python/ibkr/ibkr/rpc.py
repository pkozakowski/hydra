import dataclasses
import time
import datetime
import multiprocessing as mp
import random
import queue
import typing

from ibapi import contract as ibcontract

from ibkr import ibkr, types


DEFAULT_CASH_EXCHANGE = "IDEALPRO"
DEFAULT_WAIT = 10
DEFAULT_TIMEOUT = 60
DAY = 24 * 3600


class Server:
    def __init__(self, wait: int = DEFAULT_WAIT):
        self._symbol_cache: dict[str, list[types.Contract]] = {}
        self._wait = wait
        self._last_request_timestamp: int = 0

    def search_symbol(
        self, symbol: str, timeout: int = DEFAULT_TIMEOUT
    ) -> list[types.Contract]:
        print(f"search_symbol({repr(symbol)})")
        if symbol in self._symbol_cache:
            res = self._symbol_cache[symbol]
        else:
            [results] = self._call_method(
                "reqMatchingSymbols", [symbol], timeout=timeout
            )
            res: list[types.Contract] = [
                {
                    "id": c.conId,
                    "symbol": c.symbol,
                    "currency": c.currency,
                    "type": c.secType,
                    "exchange": c.primaryExchange,
                    "description": c.description,
                }
                for r in results
                if (c := r.contract)
            ]
            self._symbol_cache[symbol] = res
        print(f"search_symbol -> {len(res)} results")
        return res

    def fetch_cash_contract_id(
        self,
        symbol: str,
        currency: str,
        exchange: str = DEFAULT_CASH_EXCHANGE,
        timeout: int = DEFAULT_TIMEOUT,
    ) -> int | None:
        print(
            f"fetch_cash_contract_id({repr(symbol)}, {repr(currency)}, "
            f"{repr(exchange)})"
        )
        res = None
        for contract in self.search_symbol(symbol, timeout=timeout):
            if (
                contract["symbol"] == symbol
                and contract["currency"] == currency
                and contract["type"] == "CASH"
                and contract["exchange"] == exchange
            ):
                res = contract["id"]
                break
        print(f"fetch_cash_contract_id -> {repr(res)}")
        return res

    def fetch_orders_from_day_by_minute(
        self,
        contract_id: int,
        to_timestamp: int,
        exchange: str = DEFAULT_CASH_EXCHANGE,
        timeout: int = DEFAULT_TIMEOUT,
    ) -> list[types.Bar]:
        print(
            f"fetch_orders_from_day_by_minute({contract_id}, {to_timestamp}, "
            f"{repr(exchange)})"
        )
        contract = ibcontract.Contract()
        contract.conId = contract_id
        contract.exchange = exchange
        bars = self._call_method(
            "reqHistoricalData",
            [
                contract,
                timestamp_to_ibkr(to_timestamp),
                "1 D",
                "1 min",
                "BID_ASK",
                1,
                2,
                False,
                [],
            ],
            timeout=timeout,
        )
        res: list[types.Bar] = [
            {
                "timestamp": int(bar.date),
                "bid_avg": bar.open,
                "bid_min": bar.low,
                "ask_avg": bar.close,
                "ask_max": bar.high,
            }
            for bar in bars
        ]
        print(f"fetch_orders_from_day_by_minute -> {len(res)} results")
        return res

    def fetch_orders_by_minute(
        self,
        contract_id: int,
        from_timestamp: int,
        to_timestamp: int,
        exchange: str = DEFAULT_CASH_EXCHANGE,
        increment: int = 3 * 3600,
        timeout: int = DEFAULT_TIMEOUT,
    ) -> list[types.Bar]:
        print(
            f"fetch_orders_by_minute({contract_id}, {from_timestamp}, {to_timestamp}, "
            f"{repr(exchange)}, {increment})"
        )

        @dataclasses.dataclass
        class Knowledge:
            bars_by_timestamp: dict[int, types.Bar] = dataclasses.field(
                default_factory=dict
            )
            from_timestamp: int = int(1e12)
            to_timestamp: int = 0

            @property
            def null(self) -> bool:
                assert bool(self.bars_by_timestamp) == (
                    self.from_timestamp <= self.to_timestamp
                )
                return not self.bars_by_timestamp

        def probe(probe_to_timestamp: int, knowledge: Knowledge) -> None:
            response_bars = self.fetch_orders_from_day_by_minute(
                contract_id, probe_to_timestamp, exchange=exchange, timeout=timeout
            )

            for bar in response_bars:
                knowledge.bars_by_timestamp[bar["timestamp"]] = bar
                knowledge.from_timestamp = min(
                    bar["timestamp"], knowledge.from_timestamp
                )
                knowledge.to_timestamp = max(bar["timestamp"], knowledge.to_timestamp)

        knowledge = Knowledge()

        probe_to_timestamp = to_timestamp
        while knowledge.null and probe_to_timestamp < to_timestamp + DAY:
            probe(probe_to_timestamp, knowledge)
            probe_to_timestamp += increment

        probe_to_timestamp = to_timestamp - increment
        while knowledge.null and from_timestamp - DAY < probe_to_timestamp:
            probe(probe_to_timestamp, knowledge)
            probe_to_timestamp -= increment

        if not knowledge.null:
            probe_to_timestamp = knowledge.from_timestamp
            while from_timestamp < knowledge.from_timestamp:
                probe(probe_to_timestamp, knowledge)
                probe_to_timestamp = min(
                    knowledge.from_timestamp, probe_to_timestamp - increment
                )

        res = [
            knowledge.bars_by_timestamp[timestamp]
            for timestamp in sorted(knowledge.bars_by_timestamp.keys())
            if from_timestamp <= timestamp <= to_timestamp
        ]
        print(f"fetch_orders_by_minute -> {len(res)} results")
        return res

    def _call_method(
        self, method: str, args: list[typing.Any], timeout: float
    ) -> typing.Any:
        now_timestamp = datetime.datetime.utcnow().timestamp()
        wait_time = self._wait - (now_timestamp - self._last_request_timestamp)
        if wait_time > 0:
            time.sleep(wait_time)
        self._last_request_timestamp = int(datetime.datetime.utcnow().timestamp())

        response = mp.Queue()
        proc = mp.Process(target=target, args=(response, method, args))
        results = []
        try:
            proc.start()
            while True:
                result = response.get(timeout=timeout)
                if result is ibkr.EndOfResponse:
                    break
                results.append(result)
        except queue.Empty:
            raise TimeoutError
        finally:
            proc.terminate()
            proc.join(timeout=timeout)
        return results


def target(output: mp.Queue, method: str, args: list[typing.Any]) -> None:
    id = random.randrange(1000000)
    ibkr_client = ibkr.Client("127.0.0.1", 4002, id, output)
    getattr(ibkr_client, method)(0, *args)
    ibkr_client.run()


def timestamp_to_ibkr(timestamp: int) -> str:
    return datetime.datetime.fromtimestamp(timestamp).strftime("%Y%m%d-%H:%M:00")

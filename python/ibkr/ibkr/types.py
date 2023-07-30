import typing


class Contract(typing.TypedDict):
    id: int
    symbol: str
    currency: str
    type: str
    exchange: str
    description: str


class Bar(typing.TypedDict):
    timestamp: int
    bid_avg: float
    bid_min: float
    ask_avg: float
    ask_max: float

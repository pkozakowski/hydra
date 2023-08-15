import typing


class Contract(typing.TypedDict):
    id: int
    symbol: str
    currency: str
    type: str
    exchange: str
    description: str


TimeStep = tuple[int, "Bar"]


class Bar(typing.TypedDict):
    bid_avg: float
    bid_min: float
    ask_avg: float
    ask_max: float

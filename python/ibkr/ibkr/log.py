import logging
import json


class JSONFormatter(logging.Formatter):
    def format(self, record):
        return json.dumps(
            {
                "src": record.name,
                "level": record.levelname,
                "msg": record.getMessage(),
            }
        )


class Logger(logging.Logger):
    def __init__(self, name):
        super().__init__(name)
        self.setLevel(logging.DEBUG)
        handler = logging.StreamHandler()
        handler.setFormatter(JSONFormatter())
        self.addHandler(handler)

    def handle(self, record):
        msg = record.getMessage()
        level = record.levelno

        if "ibapi" in self.name:
            if level == logging.ERROR:
                # Repackage ibapi logs that should have been sent at different levels.
                if "Market data farm connection is OK" in msg:
                    level = logging.INFO
                elif "HMDS data farm connection is OK" in msg:
                    level = logging.INFO
                elif (
                    "HMDS data farm connection is inactive but should be available upon demand"
                    in msg
                ):
                    level = logging.INFO
                elif "Sec-def data farm connection is OK" in msg:
                    level = logging.INFO
                elif (
                    "The version of the application you are running" in msg
                    and "needs to be upgraded, as it will be desupported" in msg
                ):
                    level = logging.WARNING
            elif level < logging.ERROR:
                # Drop those - they're not informative for us.
                level = None

        if level is not None:
            record.levelno = level
            record.levelname = logging.getLevelName(level)
            super().handle(record)


def install():
    logging.setLoggerClass(Logger)

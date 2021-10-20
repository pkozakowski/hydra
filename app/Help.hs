module Help where

import Options.Applicative hiding (helper)
import Options.Applicative.Builder.Internal
import Options.Applicative.Types

-- Copied and adapted from Options.Applicative.

helper
    = option helpReader
    $ long "help"
   <> short 'h'
   <> help "Show this help text."
   <> value id
   <> metavar ""
   <> noArgError (ShowHelpText Nothing)
   <> hidden where
        helpReader
            = readerAbort
            . ShowHelpText
            . Just
          =<< readerAsk

hsubparser :: Mod CommandFields a -> Parser a
hsubparser m = mkParser def g reader where
    Mod _ def g = metavar "COMMAND" `mappend` m
    (groupName, cmds, subs) = mkCommand m
    reader = CmdReader groupName cmds $ fmap addHelper . subs
    addHelper pinfo = pinfo
        { infoParser = infoParser pinfo <**> helper }

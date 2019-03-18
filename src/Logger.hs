module Logger where
import qualified Data.Text                     as T
import qualified Control.Logger.Simple         as L

debugLogConf = L.LogConfig { L.lc_file   = Just "./logs/debug.log"
                                , L.lc_stderr = True
                                }
infoLogConf = L.LogConfig { L.lc_file   = Just "./logs/info.log"
, L.lc_stderr = True
}

warnLogConf = L.LogConfig { L.lc_file   = Just "./logs/warn.log"
, L.lc_stderr = True
}

errorLogConf = L.LogConfig { L.lc_file   = Just "./logs/error.log"
, L.lc_stderr = True
}

class Logger m where
    logDebug :: T.Text -> m ()
    logInfo :: T.Text -> m ()
    logWarn :: T.Text -> m ()
    logError :: T.Text -> m ()

instance Logger IO where
    logDebug = L.withGlobalLogging debugLogConf . L.logDebug
    logInfo  = L.withGlobalLogging infoLogConf . L.logInfo
    logWarn  = L.withGlobalLogging warnLogConf . L.logWarn
    logError = L.withGlobalLogging errorLogConf . L.logError

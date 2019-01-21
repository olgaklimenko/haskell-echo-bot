module Requests where
import Control.Exception
import Data.Default.Class
import Network.HTTP.Req
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T

get :: Url scheme0 -> Option scheme0 -> IO (Either String LbsResponse)
get url options = do
    rsp <- try $ runReq
        def
        (  req GET
                url
                NoReqBody
                lbsResponse
                options
        )
    pure $ either left Right rsp
    where
        left :: SomeException -> Either String a
        left httpException = Left $ show httpException

post :: (ToJSON a) => Url scheme0 -> a -> IO LbsResponse
post url body = 
    runReq def $ req POST
        url
        (ReqBodyJson body)
        lbsResponse
        mempty

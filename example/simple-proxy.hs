
import Network.HTTP.Proxy (runProxy)

-- The simplest possible HTTP/HTTPS proxy.
main :: IO ()
main = do
    putStrLn "Proxy running on port 31081. Ctrl-C to quit."
    runProxy 31081



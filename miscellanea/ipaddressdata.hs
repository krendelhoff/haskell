data IPAddress
  = IPv4 String
  | IPv4Localhost
  | IPv6 String
  | IPv6Localhost

f :: IPAddress -> String
f (IPv4 s) = s

m1 = IPv4Localhost

main :: IO ()
main = print $ str
  where
    IPAddress str = IPAddress "biba"
        -- можно вытаскивать значения из конструкторов таким образом паттерн-мэтчингом

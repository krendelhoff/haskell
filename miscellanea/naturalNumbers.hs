nats n = n : nats (n + 1)

main = print $ nats 0

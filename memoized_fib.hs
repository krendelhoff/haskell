fib = (map fi [0 ..] !!)
  where
    fi 0 = 0
    fi 1 = 1
    fi n = (fib $ n - 1) + (fib $ n - 2)

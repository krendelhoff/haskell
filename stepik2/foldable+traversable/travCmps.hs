instance (Traversable f, Traversable g) => Traversable (f |.| g) where
  traverse g (Cmps cont) = Cmps <$> (traverse (traverse g) cont)
    -- суть поднятия Cmps - мы внутрь аппликатива вновь заноняем наш двухпараметрический контейнер

import Control.Applicative

-- TODO: change Maybe with Maybe' (Maybe already defined)

instance Alternative Maybe where
    empty = Nothing

    Nothing <|> my = my
    (Just x) <|> _ = Just x
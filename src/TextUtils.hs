module TextUtils (splitWithQuotes) where

--Used for splitting the command arguments
--Will split by a whitespace, 
--but consider a single argument in case of text in quotes

splitWithQuotes = outsideQuote [] . (' ':)

add c res = if null res then [[c]] else map (++[c]) res

outsideQuote result xs = case xs of
    ' '  : ' '  : ys -> outsideQuote result $ ' ' : ys
    ' '  : '\"' : ys -> result ++ insideQuote [] ys
    ' '  : ys        -> result ++ outsideQuote [] ys
    c    : ys        -> outsideQuote (add c result) ys
    _                -> result

insideQuote result xs = case xs of
    ' '  : ' ' : ys -> insideQuote result $ ' ' : ys
    '\"' : ' ' : ys -> result ++ outsideQuote [] (' ' : ys)
    '\"' : []       -> result
    c    : ys       -> insideQuote (add c result) ys
    _               -> result
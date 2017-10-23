module TextUtils (splitWithQuotes) where

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
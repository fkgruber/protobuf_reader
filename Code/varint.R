library(bitops)
# Translated from Python:
# - https://github.com/protocolbuffers/protobuf/blob/master/python/google/protobuf/internal/encoder.py
# - https://raw.githubusercontent.com/protocolbuffers/protobuf/master/python/google/protobuf/internal/decoder.py
# Author: Fred K. Gruber <fred@gnshealthcare.com>

VarintBytes32 = function(bsize){
    bits = bitAnd(bsize, as.numeric(0x7f))
    bsize = bitShiftR(bsize, 7)
    res = c()
    while(bsize){
        res = c(res,as.raw(bitOr(as.numeric(0x80),bits)))
        bits = bitAnd(bsize, as.numeric(0x7f))
        bsize = bitShiftR(bsize, 7)
    }
    res = c(res, as.raw(bits))
    return(res)
}


DecodeVarint32 = function(buffer, pos){
    mask = (2 ^ 32 - 1)
    result = 0 %>% as.raw
    shift = 0
    while(TRUE){
        b = buffer[pos]
        result = bitOr(as.numeric(result),
                       bitShiftL(
                           bitAnd(as.numeric(b), as.numeric(0x7f)),
                           shift))
        pos = pos + 1
        if(!bitAnd(b,0x80)){
            result = bitAnd(result,mask)
            result = as.integer(result)
            return(list(result, pos))
        }
        shift = shift + 7
        if(shift >= 64){
            stop("Too  many bytes when decoding varint")
        }

    }
}

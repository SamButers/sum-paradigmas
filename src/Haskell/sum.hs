import System.IO
import System.Environment

import Data.Bits

import qualified Data.ByteString as BS

cyclicShiftR x = ( shiftR x 1 ) + ( shiftL ( (.&.) x 1 ) 15 )

sumList [] sum = sum
sumList (x:xs) sum = sumList xs (x + sum)

bsdchecksum [] checksum = checksum
bsdchecksum (x:xs) checksum = bsdchecksum xs new_checksum
                            where new_checksum = (.&.) ((cyclicShiftR checksum) + x) 0xffff

sysvchecksum bytes = ((.&.) r 0xffff) + (shiftR r 16)
                     where
                        s = sumList bytes 0
                        r = ((.&.) s 0xffff) + (shiftR ((.&.) s 0xffffffff) 16)

main = do
    args <- getArgs

    input <- openFile (args!!0) ReadMode
    byteString <- BS.hGetContents input

    let bytes = map fromIntegral (BS.unpack byteString) :: [Int]

    print $ bsdchecksum bytes 0
    print $ sysvchecksum bytes